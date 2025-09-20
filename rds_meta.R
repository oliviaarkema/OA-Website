# ============================================================
# Instacart: KPIs & Descriptive Stats for Recommendations
# ============================================================

# Setup ------------------------------------------------------
# setwd("~/Documents/RDS")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Load (readr for speed/robustness)
products   <- read_csv("products.csv", show_col_types = FALSE)
orders     <- read_csv("orders.csv", show_col_types = FALSE)
ord_prior  <- read_csv("order_products__prior.csv", show_col_types = FALSE)
aisles     <- read_csv("aisles.csv", show_col_types = FALSE)
depts      <- read_csv("departments.csv", show_col_types = FALSE)

# Helpers ----------------------------------------------------
safe_div <- function(x, y, eps = 1e-9) x / pmax(y, eps)

# Prep: prior orders + product meta --------------------------
orders_prior <- orders %>%
  filter(eval_set == "prior") %>%
  select(order_id, user_id, order_number, order_dow, order_hour_of_day, days_since_prior_order)

prod_meta <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(depts,  by = "department_id") %>%
  select(product_id, product_name, aisle, department)

ordlines <- ord_prior %>%
  inner_join(orders_prior, by = "order_id") %>%
  inner_join(prod_meta,   by = "product_id") %>%
  select(order_id, user_id, product_id, product_name, aisle, department,
         reordered, order_dow, order_hour_of_day, days_since_prior_order)

# ============= 1) Core KPIs =================================
total_orders <- n_distinct(ordlines$order_id)
total_users  <- n_distinct(ordlines$user_id)
total_prods  <- n_distinct(ordlines$product_id)

basket_items <- ordlines %>% count(order_id, name = "items_in_basket")
avg_items_per_order    <- mean(basket_items$items_in_basket)
median_items_per_order <- median(basket_items$items_in_basket)
pct_single_item_orders <- mean(basket_items$items_in_basket == 1)

overall_reorder_rate <- mean(ordlines$reordered, na.rm = TRUE)
avg_days_between_orders <- mean(orders_prior$days_since_prior_order, na.rm = TRUE)
median_days_between_orders <- median(orders_prior$days_since_prior_order, na.rm = TRUE)

kpis <- tibble::tibble(
  total_orders,
  total_users,
  total_prods,
  avg_items_per_order,
  median_items_per_order,
  pct_single_item_orders,
  overall_reorder_rate,
  avg_days_between_orders,
  median_days_between_orders
)

# ============= 2) Timing (Send/Promo windows) ===============
dow_dist <- orders_prior %>%
  count(order_dow, name = "orders") %>%
  mutate(share = orders / sum(orders))

hour_dist <- orders_prior %>%
  count(order_hour_of_day, name = "orders") %>%
  mutate(share = orders / sum(orders))

dow_hour_heat <- orders_prior %>%
  count(order_dow, order_hour_of_day, name = "orders") %>%
  group_by(order_dow) %>%
  mutate(share_within_dow = orders / sum(orders)) %>%
  ungroup()

# ============= 3) Product Metrics ===========================
# Per-product frequency, penetration, reorder rate
prod_metrics <- ordlines %>%
  distinct(order_id, product_id, .keep_all = TRUE) %>%
  group_by(product_id, product_name, aisle, department) %>%
  summarise(
    orders_with_prod = n(),
    pen_orders = orders_with_prod / total_orders,
    reorder_rate = mean(reordered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(orders_with_prod))

# Coverage curve for top-N (how much of activity top items capture)
prod_counts <- ordlines %>%
  count(product_id, product_name, sort = TRUE, name = "line_items")

total_line_items <- sum(prod_counts$line_items)
coverage_curve <- prod_counts %>%
  mutate(rank = row_number()) %>%
  mutate(cum_items = cumsum(line_items),
         cum_share = cum_items / total_line_items)

# ============= 4) Anchor & Attachment (Assoc. Rules Light) ===
TOP_ANCHORS <- 50  # change if desired
anchors <- prod_metrics %>%
  slice_head(n = TOP_ANCHORS) %>%
  transmute(anchor_id = product_id, anchor_name = product_name)

# supports for all products (order-level)
op <- ordlines %>% distinct(order_id, product_id)
prod_support <- op %>%
  count(product_id, name = "orders_with_prod") %>%
  mutate(support = orders_with_prod / total_orders)

# anchor -> product co-occurrence
cooc <- op %>%
  inner_join(anchors, by = c("product_id" = "anchor_id")) %>%
  rename(anchor_id = product_id) %>%
  inner_join(op, by = "order_id") %>%
  filter(anchor_id != product_id) %>%
  count(anchor_id, product_id, anchor_name, name = "orders_with_both") %>%
  left_join(prod_support %>% select(product_id, orders_with_prod, support),
            by = c("anchor_id" = "product_id")) %>%
  rename(orders_with_anchor = orders_with_prod, support_anchor = support) %>%
  left_join(prod_support %>% select(product_id, orders_with_prod, support),
            by = "product_id") %>%
  rename(orders_with_prod = orders_with_prod, support_prod = support) %>%
  mutate(
    support_both      = orders_with_both / total_orders,
    confidence_A_to_B = safe_div(orders_with_both, orders_with_anchor),
    lift              = safe_div(support_both, support_anchor * support_prod)
  ) %>%
  left_join(products %>% select(product_id, product_name), by = "product_id")

# Top pairs & per-anchor recommendations
TOP_PAIRS <- 50
top_pairs_by_lift <- cooc %>%
  arrange(desc(lift), desc(confidence_A_to_B), desc(orders_with_both)) %>%
  slice_head(n = TOP_PAIRS)

TOP_ATTACH <- 5
anchor_recs_by_lift <- cooc %>%
  group_by(anchor_id, anchor_name) %>%
  slice_max(order_by = lift, n = TOP_ATTACH, with_ties = FALSE) %>%
  ungroup()

anchor_recs_by_conf <- cooc %>%
  group_by(anchor_id, anchor_name) %>%
  slice_max(order_by = confidence_A_to_B, n = TOP_ATTACH, with_ties = FALSE) %>%
  ungroup()

# ============= 5) Aisle / Department Views ==================
dept_penetration <- ordlines %>%
  distinct(order_id, department) %>%
  count(department, name = "orders_with_dept") %>%
  mutate(pen_orders = orders_with_dept / total_orders) %>%
  arrange(desc(pen_orders))

aisle_penetration <- ordlines %>%
  distinct(order_id, aisle) %>%
  count(aisle, name = "orders_with_aisle") %>%
  mutate(pen_orders = orders_with_aisle / total_orders) %>%
  arrange(desc(pen_orders))

# Cross-department co-occurrence (strength of cross-sell by dept)
dept_op <- ordlines %>% distinct(order_id, department)
dept_cooc <- dept_op %>%
  rename(deptA = department) %>%
  inner_join(dept_op, by = "order_id") %>%
  rename(deptB = department) %>%
  filter(deptA != deptB) %>%
  count(deptA, deptB, name = "orders_with_both") %>%
  left_join(dept_op %>% count(deptA = department, name = "orders_with_deptA"), by = "deptA") %>%
  left_join(dept_op %>% count(deptB = department, name = "orders_with_deptB"), by = "deptB") %>%
  mutate(
    support_both = orders_with_both / total_orders,
    lift = safe_div(
      support_both,
      (orders_with_deptA / total_orders) * (orders_with_deptB / total_orders)
    )
  ) %>%
  arrange(desc(lift), desc(orders_with_both))

# ============= 6) Write CSVs for the write-up ===============
readr::write_csv(kpis,                     "out_kpis.csv")
readr::write_csv(dow_dist,                 "out_timing_dayofweek.csv")
readr::write_csv(hour_dist,                "out_timing_hour.csv")
readr::write_csv(dow_hour_heat,            "out_timing_day_hour_heat.csv")
readr::write_csv(prod_metrics,             "out_product_metrics.csv")
readr::write_csv(coverage_curve,           "out_topN_coverage_curve.csv")
readr::write_csv(top_pairs_by_lift,        "out_top_pairs_by_lift.csv")
readr::write_csv(anchor_recs_by_lift,      "out_anchor_recs_by_lift.csv")
readr::write_csv(anchor_recs_by_conf,      "out_anchor_recs_by_conf.csv")
readr::write_csv(dept_penetration,         "out_department_penetration.csv")
readr::write_csv(aisle_penetration,        "out_aisle_penetration.csv")
readr::write_csv(dept_cooc,                "out_department_cooccurrence.csv")

# ============= 7) Console snapshots (handy in notes) ========
message("\n--- CORE KPIs ---"); print(kpis)
message("\n--- Timing: Day-of-Week (share) ---"); print(dow_dist)
message("\n--- Timing: Hour-of-Day (share) ---"); print(hour_dist)
message("\n--- Product Metrics (head) ---"); print(head(prod_metrics, 20))
message("\n--- Coverage Curve (head) ---"); print(head(coverage_curve, 20))
message("\n--- Top Pairs by Lift (head) ---"); print(head(top_pairs_by_lift, 20))
message("\n--- Anchor Recs by Lift (head) ---"); print(head(anchor_recs_by_lift, 20))
message("\n--- Anchor Recs by Confidence (head) ---"); print(head(anchor_recs_by_conf, 20))
message("\n--- Department Penetration (head) ---"); print(head(dept_penetration, 20))
message("\n--- Department Co-occurrence (head) ---"); print(head(dept_cooc, 20))
