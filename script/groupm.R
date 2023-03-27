library("DBI")
library("RSQLite")
library("tidyverse")
library("lme4")
library("plm")
library("tidymodels")
library("xgboost")
library("tune")
library("finetune")
library("yardstick")
library("stargazer")



# Access database
db <- dbConnect(dbDriver("SQLite"), dbname = "../data/data.db")

# Convert table in database to dataframe
data <- tbl(db, "merged_data")
data_df <- collect(data)

# End connection to database
dbDisconnect(db)


# Set data types for date and factor columns
data_df <- data_df %>% mutate(
    across(c(date, anal_cat, sub_cat, prod_anal_vert), as.factor)
)

# Exclude variables from analysis
data_df <- data_df %>% select(-c(online_mkt, sem, content_mkt))

data_df$quarter <- paste0(
    lubridate::year(as.Date(data_df$date)),
    "Q",
    lubridate::quarter(as.Date(data_df$date))
)

data_df$quarter <- as.factor(data_df$quarter)

data_df <- data_df %>%
    group_by(prod_anal_vert, quarter) %>%
    mutate(sales_days_per_quarter = sum(sales_days_per_month)) %>%
    ungroup()

data_df <- data_df %>%
    rename(sales_days = sales_days_per_month) %>%
    group_by(anal_cat, prod_anal_vert, quarter) %>%
    summarise(
        across(c(sales_days), sum),
        across(c(sum_gmv, avg_sla:nps), mean)
    ) %>%
    ungroup()

################### Exploration plots
data_df %>%
    mutate(sum = tv + digital + sponsorship + affiliates + radio + other) %>%
    mutate(sum_cat = cut(sum, 6)) %>%
    # filter(anal_cat != "Camera") %>%
    ggplot(aes(sum_cat, log(sum_gmv))) +
    geom_boxplot() +
    coord_flip() +
    labs(x = "Expenditure bracket", y = "log(Revenue)") +
    scale_x_discrete(position = "top")

ggsave("../store/plot1.jpg", device = "jpg")


data_df %>%
    mutate(sum = tv + digital + sponsorship + affiliates + radio + other) %>%
    mutate(sum_cat = cut(sum, 5)) %>%
    # filter(anal_cat != "Camera") %>%
    ggplot(aes(sum_cat, log(sum_gmv), group = anal_cat)) +
    facet_wrap(. ~ quarter) +
    stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", na.rm = TRUE, mapping = aes(color = anal_cat), position = "jitter") +
    theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
    labs(
        color = "Product Category",
        x = "Expenditure bracket",
        y = "log(Revenue)"
    )

ggsave("../store/plot2.jpg", device = "jpg")

data_df %>%
    mutate(sum = tv + digital + sponsorship + affiliates + radio + other) %>%
    mutate(sum_cat = cut(sum, 4)) %>%
    # filter(anal_cat != "Camera") %>%
    ggplot(aes(log(sum_gmv), after_stat(density), fill = sum_cat)) +
    geom_histogram(bins = 10) +
    scale_fill_ordinal() +
    labs(
        x = "log(Revenue)",
        y = "Density",
        fill = "Expenditure bracket"
    )

ggsave("../store/plot3.jpg", device = "jpg")

###################


# Create formulas for each of the models
dependent_var <- data_df %>%
    select(sum_gmv) %>%
    colnames()

log_vars <- data_df %>%
    select(tv:other) %>%
    colnames()

other_ind_vars <- data_df %>%
    select(
        -c(
            #            id,
            quarter,
            # date,
            sum_gmv,
            tv:other,
            anal_cat,
            # sub_cat,
            prod_anal_vert
        )
    ) %>%
    colnames()

grouping_vars <- data_df %>%
    select(
        c(
            # date,
            quarter,
            anal_cat
            # sub_cat,
            # prod_anal_vert
        )
    ) %>%
    colnames()

log_terms <- c()

for (i in log_vars) {
    term <- paste0("log(", i, "+0.5)")
    log_terms[i] <- term
}

# log_terms <- paste0("log(I(", paste(log_vars, collapse = "+"), "))")

random_terms <- c()

for (i in grouping_vars) {
    term <- paste0("(1|", i, ")")
    random_terms[i] <- term
}

random_terms <- paste(random_terms, collapse = "+")

covariates_me <- paste(
    c(
        log_terms,
        other_ind_vars,
        random_terms
    ),
    collapse = "+"
)

covariates_plm <- paste(
    c(
        log_terms,
        other_ind_vars
    ),
    collapse = "+"
)

formula_me <- paste(c(paste0("log(", dependent_var, "+0.5)"), covariates_me), collapse = "~")
formula_plm <- paste(c(paste0("log(", dependent_var, "+0.5)"), covariates_plm), collapse = "~")




# Fit mixed effects model
fit_me <- lmer(formula_me, data = data_df)
fit_me_confint <- confint(fit_me, method = "boot")

# Fit two-way fixed effects model
fit_plm <- plm(
    formula_plm,
    data_df,
    index = c("anal_cat", "quarter"),
    model = "within",
    effect = "twoways"
)

stargazer(confint(fit_plm), title = "Two-Way Fixed Effects Model")
stargazer(fit_me_confint, title = "Mixed Effects Model")


# Generate XGBoost model
data_df <- data_df %>% mutate(across(!c(anal_cat, prod_anal_vert, quarter), as.numeric))

rec <- recipe(sum_gmv ~ ., data = data_df) %>%
    step_rm(prod_anal_vert, quarter, anal_cat) %>%
    update_role(sum_gmv, new_role = "outcome") %>%
    # update_role(c(quarter, anal_cat), new_role = "cat_pred") %>%
    update_role(
        c(
            sales_days,
            avg_sla,
            avg_proc_sla,
            tv,
            digital,
            sponsorship,
            affiliates,
            radio,
            other,
            nps
        ),
        # new_role = "num_pred"
    ) %>%
    # step_dummy(
    #     quarter,
    #     anal_cat,
    #     one_hot = TRUE
    # ) %>%
    step_normalize(has_role("outcome")) %>%
    step_normalize(has_role("predictor"))

spec <- boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    sample_size = tune(),
    stop_iter = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
) %>%
    set_engine("xgboost", nthread = 3) %>%
    set_mode("regression")

workflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

xgboost_params <- hardhat::extract_parameter_set_dials(workflow) %>% finalize(data_df)

set.seed(10)
validation_split <- vfold_cv(data_df, v = 5)

# xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 500)))

cl <- parallel::makeCluster(3)
set.seed(10)
result <- tune::tune_bayes(workflow,
    initial = 10,
    param_info = xgboost_params,
    iter = 50,
    metrics = yardstick::metric_set(mae),
    resamples = validation_split,
    control = tune::control_bayes(save_pred = TRUE, save_workflow = TRUE, no_improve = 10)
)
parallel::stopCluster(cl)

result %>%
    collect_metrics() %>%
    arrange(mean) %>%
    print()

# Select best model
best <- select_best(result, metric = "mae")
# Finalize the workflow with those parameter values
final_wf <- workflow %>% finalize_workflow(best)


fit <- final_wf %>%
    fit(data_df)

# See variables arranged by importance
fit %>%
    extract_fit_parsnip() %>%
    vip::vi() %>%
    stargazer(summary = FALSE, rownames = FALSE, title = "XGBoost model")


# Save workflow
saveRDS(final_wf, "../store/bestwf_xgb.Rds")

# Save result
saveRDS(result, "../store/result_xgb.Rds")
