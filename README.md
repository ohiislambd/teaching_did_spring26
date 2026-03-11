Difference-in-Differences Tutorial
================
Ohi Islam
2026-03-11

# Introduction

When introducing the difference-in-differences (DiD) method in Economics
of Education, we begin with intuition before turning
to regression. This tutorial follows a step-by-step approach. We first
examine outcomes visually, then organize them in a simple table, and
finally connect that setup to a regression model. The goal is to help
you understand how the DiD estimate captures the additional change
in outcomes for the treated group relative to the control group.

# Learning Goals

By the end of this tutorial, students should be able to:

- describe the logic of a DiD design
- compute a DiD estimate using average outcomes
- interpret the interaction term in a regression model
- connect graphical, tabular, and regression-based interpretations

\##A Simple Example

Suppose we are studying the effect of an education policy introduced for
one group of schools but not another. We observe average outcomes for
treatment and control groups before and after the policy.

# Setup

``` r
library(tidyverse)
library(knitr)
```

# Visual Intuition

Suppose one group is exposed to a policy intervention and another group
is not. We observe outcomes for both groups before and after the
intervention.

``` r
did_df <- tibble(
  group = c("Control", "Control", "Treatment", "Treatment"),
  period = c("Pre", "Post", "Pre", "Post"),
  outcome = c(50, 54, 48, 60)
)

did_df
```

    ## # A tibble: 4 × 3
    ##   group     period outcome
    ##   <chr>     <chr>    <dbl>
    ## 1 Control   Pre         50
    ## 2 Control   Post        54
    ## 3 Treatment Pre         48
    ## 4 Treatment Post        60

We can visualize these outcomes first.

``` r
ggplot(did_df, aes(x = period, y = outcome, fill = group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Outcomes by Group and Period",
    x = "Period",
    y = "Average Outcome"
  )
```

![](DiD_example_on_git_v2_files/figure-gfm/plot-bar-1.png)<!-- -->

The graph allows us to compare changes over time for the treatment and
control groups. The control group improves somewhat between the
pre-treatment and post-treatment periods, while the treatment group
improves by more. The key question is whether the additional improvement
in the treatment group can be attributed to the policy.

# Constructing the DiD Estimate

The DiD estimate compares:

1.  the change in the treatment group
2.  the change in the control group
3.  the difference between those two changes

## Step 1: Compute Within-Group Changes

Now compute the change over time within each group.

``` r
control_pre  <- 50
control_post <- 54
treat_pre    <- 48
treat_post   <- 60

control_change <- control_post - control_pre
treat_change   <- treat_post - treat_pre
did_estimate   <- treat_change - control_change

control_change
```

    ## [1] 4

``` r
treat_change
```

    ## [1] 12

``` r
did_estimate
```

    ## [1] 8

The control group changed by 4, and the treatment group changed by 12.

So the DiD estimate is:

$$
(60 - 48) - (54 - 50) = 12 - 4 = 8
$$

## Step 2: Show the Calculation in a Table

We now simulate individual-level data so we can estimate the same idea
using regression.

``` r
did_summary <- tibble(
  Group = c("Control", "Treatment"),
  Pre = c(control_pre, treat_pre),
  Post = c(control_post, treat_post),
  Change = c(control_change, treat_change)
)

kable(did_summary, caption = "Outcomes and Changes by Group")
```

| Group     | Pre | Post | Change |
|:----------|----:|-----:|-------:|
| Control   |  50 |   54 |      4 |
| Treatment |  48 |   60 |     12 |

Outcomes and Changes by Group

This table shows that the treatment group improved more than the control
group, and the extra improvement is the DiD estimate.

Note that the Control Group experiences a increase by 4 points in the outcome variable between the pre- and post-treatment periods.
Note that the Control Group already has a 2-point advantage over the treatment group in the outcome in the pre=treatment period. 


# Regression Interpretation

We now connect this idea to a regression model.

$$
Y_i = \beta_0 + \beta_1 \text{Treatment}_i + \beta_2 \text{Post}_i + \beta_3 (\text{Treatment}_i \times \text{Post}_i) + \varepsilon_i
$$

The coefficient $\beta_3$ is the DiD estimate.

## Simulated Individual-Level Data

To fit the regression, we create a simple individual-level dataset that
matches the same group averages.

``` r
set.seed(123)

n <- 100

sim_df <- tibble(
  treatment = rep(c(0, 0, 1, 1), each = n),
  post = rep(c(0, 1, 0, 1), each = n),
  mean_outcome = c(
    rep(50, n),
    rep(54, n),
    rep(48, n),
    rep(60, n)
  )
) %>%
  mutate(
    outcome = rnorm(n = 4 * n, mean = mean_outcome, sd = 4)
  )

head(sim_df)
```

    ## # A tibble: 6 × 4
    ##   treatment  post mean_outcome outcome
    ##       <dbl> <dbl>        <dbl>   <dbl>
    ## 1         0     0           50    47.8
    ## 2         0     0           50    49.1
    ## 3         0     0           50    56.2
    ## 4         0     0           50    50.3
    ## 5         0     0           50    50.5
    ## 6         0     0           50    56.9

## Estimate the DiD Regression

``` r
did_model <- lm(outcome ~ treatment + post + treatment:post, data = sim_df)

summary(did_model)
```

    ## 
    ## Call:
    ## lm(formula = outcome ~ treatment + post + treatment:post, data = sim_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.7187 -2.6300 -0.1994  2.6781 13.3943 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     50.3616     0.3873 130.040  < 2e-16 ***
    ## treatment       -1.8798     0.5477  -3.432 0.000662 ***
    ## post             3.2082     0.5477   5.858 9.88e-09 ***
    ## treatment:post   8.1651     0.7746  10.542  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.873 on 396 degrees of freedom
    ## Multiple R-squared:  0.557,  Adjusted R-squared:  0.5536 
    ## F-statistic:   166 on 3 and 396 DF,  p-value: < 2.2e-16

# Interpreting the Coefficients

In this regression:

- the intercept is the average outcome for the control group in the pre
  period.
- the coefficient on `treatment` is approximately the difference between treatment and
  control in the pre period. In the table of average differences, the $treatment\\ group\\ average - control\\ group\\ average$
  in the pre-treatment period was $-2$.
- the coefficient on `post` is the change over time for the control
  group. In the table of average differences, we saw that the $natural$ difference in outcome for the control group is a 4-point change.
- the coefficient on `treatment:post` is the DiD estimate

In the regression section we did not use deterministic outcomes. Instead, we simulated individual-level data with random variation around the group means:

```r
outcome = rnorm(n = 4 * n, mean = mean_outcome, sd = 4)
```

This means the data generating process can be written as
$$
Y_{igt} = μ_{gt} + ε_{igt}
$$
where $μ_{gt}$ represents the group-period mean (50, 54, 48, or 60) and $ε_{igt}$ is a random disturbance.

Because the outcomes include random variation, the sample averages in the simulated data are not exactly equal to the theoretical means. For example, the simulated control-group pre-period mean might be 50.36 instead of exactly 50.

The regression therefore estimates the sample averages, not the theoretical values from the table. As a result:

- the coefficient on `treatment` will be close to −2, but not exactly −2.
- the coefficient on `post` will be close to 4, but not exactly 4.
- the coefficient on `treatment:post` will be close to 8, but not exactly 8.

This is exactly what we expect in a stochastic model. In expectation, the regression still recovers the theoretical difference-in-differences effect, so the expected value of the interaction coefficient is 8. The realized estimate differs slightly because the simulated data contain random noise.

## Extract the Key Coefficient

``` r
coef(did_model)["treatment:post"]
```

    ## treatment:post 
    ##       8.165059

This interaction coefficient represents the additional post-period
change in the treated group relative to the control group.

# Linking the Graph, Table, and Regression

The same treatment effect appears in three ways:

- in the graph, as the larger increase for the treatment group
- in the table, as the difference between the two group-specific changes
- in the regression, as the interaction coefficient

# Parallel Trends Assumption

For a DiD design to have a causal interpretation, we usually rely on the
parallel trends assumption.

This means that, in the absence of treatment, the treatment and control
groups would have followed similar trends over time.

This assumption cannot be tested directly for the treated post-treatment
outcome, but researchers often examine pre-treatment trends and
institutional context to assess whether it is plausible.

# Conclusion

Difference-in-differences estimates the treatment effect by comparing
changes over time across treated and untreated groups. The key parameter
is the coefficient of the interaction between treatment status and the post-treatment
period.

# Optional Exercise

Try modifying the four group-period averages and see how the DiD
estimate changes.

For example:

- increase the post-treatment outcome for the treatment group
- decrease the post-treatment outcome for the control group
- make both groups trend upward by the same amount

How does each change affect the DiD estimate?







