# 필요한 패키지 설치 및 로드
if (!require(tableone)) install.packages("tableone")
if (!require(dplyr)) install.packages("dplyr")
if (!require(survival)) install.packages("survival")
if (!require(survminer)) install.packages("survminer")
if (!require(survIDINRI)) install.packages("survIDINRI")
if (!require(rms)) install.packages("rms")
if (!require(Hmisc)) install.packages("Hmisc")
if (!require(pROC)) install.packages("pROC")
if (!require(timeROC)) install.packages("timeROC")

library(tableone)
library(dplyr)
library(survival)
library(survminer)
library(survIDINRI)
library(rms)
library(Hmisc)
library(pROC)
library(timeROC)

# 현재 작업 디렉토리 설정
setwd("C:/Users/Shin Dong Ho/OneDrive/2025_ASN_Abstract_huston")
# C:\Users\isaac\OneDrive\2025_ASN_Abstract_huston
"C:/Users/Shin Dong Ho/OneDrive/2025_ASN_Abstract_huston"

# 데이터 읽기
data <- read.csv("FInal_modified_data_4_23.csv", 
                 header = TRUE, 
                 stringsAsFactors = TRUE)

# --- 데이터 전처리 ---
cat("\n--- 데이터 전처리 시작 ---\n")

# MACE 관련 변수 타입 변환
cat("MACE 관련 변수 타입 변환 전:
")
print(str(data[, c("MACE_or_fu_date_month", "MACE")]))
data$MACE_or_fu_date_month <- as.numeric(data$MACE_or_fu_date_month)
data$MACE <- as.integer(data$MACE) 
cat("MACE 관련 변수 타입 변환 후:
")
print(str(data[, c("MACE_or_fu_date_month", "MACE")]))

# 변환 후 NA 값 확인
if (any(is.na(data$MACE_or_fu_date_month)) | any(is.na(data$MACE))) {
  warning("데이터 타입 변환 후 NA 값이 생성되었습니다. 원본 데이터를 확인하세요.")
}

# 범주형 변수 factor 변환
data$EKG_Rhythm_avR <- factor(data$EKG_Rhythm_avR)
data$High_EEa <- factor(data$High_EEa)

# 변수 목록 정의
# Table 1에 사용될 변수 그룹 정의
vars <- list(
    demographics = c("Age", "Gender", "Height", "Weight", "BMI", "Dialysis_vintage_month"),
    comorbidities = c("DM", "HTN", "Prior_CVD", "CAD", "CVA", "PAD", "smoker"),
    medications = c("ACEIARB", "CCB", "BB", "Diuretics", "ASA", "Statin", "VitD", 
                   "Ca_P_binder", "Non_Ca_P_binder", "PhosphateBinder", "DM_medication", "Insulin"),
    lab_values = c("SBP", "DBP", "WBC", "Hb", "Hc_t", "BUN", "Cr", "GFR", "Albumin",
                   "Ca", "P", "Chol", "Triglyceride", "HDL", "LDL", "CRP", "iPTH", 
                   "KT_V", "Fe", "TIBC", "Ferritin"),
    cardiac = c("QRS_duration_msec", "QTc_interval_msec", "EF", "Heart_failure",
                "LVm", "LVMI", "E_Ea", "High_EEa")
)
# 실제 데이터에 존재하는 변수만 선택 (Table 1 용)
existing_vars <- intersect(unlist(vars), names(data))


# 이진 변수 목록 정의
binary_vars <- c("Gender", "DM", "HTN", "Prior_CVD", "CAD", "CVA", "PAD", "smoker",
                 "ACEIARB", "CCB", "BB", "Diuretics", "ASA", "Statin", "VitD",
                 "Ca_P_binder", "Non_Ca_P_binder", "PhosphateBinder", "DM_medication", "Insulin",
                 "Heart_failure", "High_EEa", "EKG_Rhythm_avR")
# 실제 데이터에 존재하는 이진 변수만 선택 (Factor 변환용)
factor_vars <- intersect(binary_vars, existing_vars)

# Table 1 및 정규성 검정에 사용될 변수 순서 정의
ordered_vars <- c(
  "Age", "Gender", "BMI", "Dialysis_vintage_month", "DM", "HTN", "Prior_CVD",
  "Hb", "Albumin", "Ca", "P", "Chol", "CRP", "iPTH_per_100", 
  "QRS_duration_msec", "QTc_interval_msec", "EF", "Heart_failure", "LVMI", "E_Ea", "High_EEa",
  "ACEIARB", "CCB", "BB", "Diuretics", "ASA", "Statin", "VitD", "PhosphateBinder"
)
# 실제 데이터에 존재하는 변수만 선택
existing_ordered_vars <- intersect(ordered_vars, names(data))
# 연속형 변수 목록 (정규성 검정용)
continuous_ordered_vars <- setdiff(existing_ordered_vars, factor_vars)

cat("--- 데이터 전처리 완료 ---\n")

# --- 연속형 변수 정규성 검정 ---
cat("\n--- 연속형 변수 정규성 검정 시작 ---\n")
normality_results <- data.frame(Variable = character(), Shapiro_p = numeric(), Is_Normal = logical(), stringsAsFactors = FALSE)
groups <- levels(data$EKG_Rhythm_avR)

for (var in continuous_ordered_vars) {
  if (sum(!is.na(data[[var]])) > 3) {
    sw_test_all <- shapiro.test(data[[var]])
    is_normal_all_groups <- TRUE
    for (grp in groups) {
      group_data <- data[data$EKG_Rhythm_avR == grp, ]
      if (sum(!is.na(group_data[[var]])) > 3) {
        sw_test_group <- shapiro.test(group_data[[var]])
        if (sw_test_group$p.value < 0.05) { is_normal_all_groups <- FALSE }
      } else { is_normal_all_groups <- FALSE }
    }
    is_normal <- sw_test_all$p.value >= 0.05 && is_normal_all_groups
    normality_results <- rbind(normality_results, data.frame(Variable = var, Shapiro_p = sw_test_all$p.value, Is_Normal = is_normal))
  }
}

# 정규성 검정 결과 출력
print("정규성 검정 결과:")
print(normality_results)
non_normal_vars <- normality_results$Variable[!normality_results$Is_Normal]
print("비정규 분포 변수:")
print(non_normal_vars)
print("정규 분포 변수:")
print(setdiff(normality_results$Variable, non_normal_vars))
cat("--- 연속형 변수 정규성 검정 완료 ---\n")

# --- Table 1 생성 ---
cat("\n--- Table 1 생성 시작 ---\n")

# Strata별 Table 1 생성 (p-value 계산용)
table1 <- CreateTableOne(vars = existing_vars, strata = "EKG_Rhythm_avR", data = data, factorVars = factor_vars, test = TRUE)
# Overall Table 1 생성
table1_overall <- CreateTableOne(vars = existing_vars, data = data, factorVars = factor_vars, test = FALSE)

# 결과 매트릭스 생성 (비정규 변수 지정)
table1_strata_matrix <- print(table1, nonnormal = non_normal_vars, minMax = TRUE, printToggle = FALSE, showAllLevels = TRUE, digits = 1, pDigits = 3)
table1_overall_matrix <- print(table1_overall, nonnormal = non_normal_vars, minMax = TRUE, printToggle = FALSE, showAllLevels = TRUE, digits = 1, pDigits = 3)

# Overall, Strata, p-value 열 결합
table1_combined_matrix <- cbind(Overall = table1_overall_matrix[, "Overall"], table1_strata_matrix[, c("0", "1", "p")])

# CSV 파일로 저장
write.csv(table1_combined_matrix, "Table1_EKG_Rhythm_avR_with_Overall.csv")
cat("Table 1 결과가 'Table1_EKG_Rhythm_avR_with_Overall.csv' 파일로 저장되었습니다.\n")

# --- 생존 분석 (Kaplan-Meier) ---
cat("\n--- Kaplan-Meier 분석 시작 ---\n")

# Survival 객체 생성
surv_obj <- Surv(time = data$MACE_or_fu_date_month, event = data$MACE)

# Kaplan-Meier 분석 수행
km_fit <- survfit(surv_obj ~ EKG_Rhythm_avR, data = data)

# Kaplan-Meier 곡선 생성 및 저장
km_plot <- ggsurvplot(km_fit, data = data, risk.table = TRUE, pval = TRUE, conf.int = TRUE,
                      xlab = "Time (months)", ylab = "MACE-free Survival Probability",
                      risk.table.height = 0.25, ggtheme = theme_bw(), palette = "jco")
ggsave("KM_curve_EKG_Rhythm_avR.png", plot = km_plot$plot, width = 10, height = 8, dpi = 300)
cat("Kaplan-Meier 곡선이 'KM_curve_EKG_Rhythm_avR.png' 파일로 저장되었습니다.\n")

# 생존율 요약 출력
print("Kaplan-Meier 생존율 요약 (그룹별):")
print(summary(km_fit))
print("Kaplan-Meier 생존율 요약 (1, 3, 5년):")
summary(km_fit, times = c(12, 36, 60))

cat("--- Kaplan-Meier 분석 완료 ---\n")

# --- 추적 관찰 기간 및 MACE 발생률 분석 ---
cat("\n--- 추적 관찰 기간 및 MACE 발생률 분석 시작 ---\n")

# 추적 기간 정규성 검정
shapiro_test <- shapiro.test(data$MACE_or_fu_date_month)
is_normal <- shapiro_test$p.value > 0.05

# 추적 기간 통계량 계산 (월/년)
mean_follow_up_months <- mean(data$MACE_or_fu_date_month)
median_follow_up_months <- median(data$MACE_or_fu_date_month)
iqr_follow_up_months <- IQR(data$MACE_or_fu_date_month)
sd_follow_up_months <- sd(data$MACE_or_fu_date_month)
mean_follow_up_years <- mean_follow_up_months / 12
median_follow_up_years <- median_follow_up_months / 12

# 결과 출력
cat("추적 관찰 기간 분석 결과:\n")
print(sprintf("  Shapiro-Wilk p-value: %.3f", shapiro_test$p.value))
if (is_normal) {
  print(sprintf("  정규 분포 따름 (p > 0.05). 평균 ± 표준편차: %.1f ± %.1f 개월 (%.1f ± %.1f 년)", 
                mean_follow_up_months, sd_follow_up_months, mean_follow_up_years, sd_follow_up_months / 12))
} else {
  print(sprintf("  정규 분포 따르지 않음 (p ≤ 0.05). 중앙값 [IQR]: %.1f [%.1f-%.1f] 개월 (%.1f [%.1f-%.1f] 년)", 
                median_follow_up_months, quantile(data$MACE_or_fu_date_month, 0.25), quantile(data$MACE_or_fu_date_month, 0.75),
                median_follow_up_years, quantile(data$MACE_or_fu_date_month, 0.25)/12, quantile(data$MACE_or_fu_date_month, 0.75)/12))
}
print(sprintf("  범위: %.1f ~ %.1f 개월", min(data$MACE_or_fu_date_month), max(data$MACE_or_fu_date_month)))

# MACE 발생률 계산
total_person_years <- sum(data$MACE_or_fu_date_month) / 12
total_mace_events <- sum(data$MACE)
mace_rate_per_100_person_years <- (total_mace_events / total_person_years) * 100

person_years_by_group <- tapply(data$MACE_or_fu_date_month, data$EKG_Rhythm_avR, sum) / 12
mace_events_by_group <- tapply(data$MACE, data$EKG_Rhythm_avR, sum)
patients_by_group <- table(data$EKG_Rhythm_avR)
mace_proportion_by_group <- (mace_events_by_group / patients_by_group) * 100
mace_rate_by_group <- (mace_events_by_group / person_years_by_group) * 100

# 결과 출력
cat("
MACE 발생 분석 결과:
")
print(sprintf("  추적 관찰 기간 중앙값: %.1f 개월 (약 %.1f 년)", median_follow_up_months, median_follow_up_months/12))
print("  그룹별 MACE 발생:")
for (group in names(mace_events_by_group)) {
  print(sprintf("    %s: %d/%d 명 (%.1f%%)", group, mace_events_by_group[group], patients_by_group[group], mace_proportion_by_group[group]))
}
print("  그룹별 MACE 발생률 (100인년당):")
for (group in names(mace_rate_by_group)) {
  print(sprintf("    %s: %.2f (%.1f 인년, %d건)", group, mace_rate_by_group[group], person_years_by_group[group], mace_events_by_group[group]))
}

# 발생률 차이 검정 (Poisson test)
poisson_test <- poisson.test(c(mace_events_by_group[1], mace_events_by_group[2]), 
                           c(person_years_by_group[1], person_years_by_group[2]),
                           alternative = "two.sided")
print("  발생률 차이 통계적 유의성:")
print(sprintf("    P-value = %.3f", poisson_test$p.value))
print(sprintf("    발생률 비 (Rate Ratio): %.2f (95%% CI: %.2f-%.2f)", 
             poisson_test$estimate, poisson_test$conf.int[1], poisson_test$conf.int[2]))

print(sprintf("  전체 MACE 발생률 (100인년당): %.2f (%.1f 인년, %d건)", 
              mace_rate_per_100_person_years, total_person_years, total_mace_events))

cat("--- 추적 관찰 기간 및 MACE 발생률 분석 완료 ---\n")

# --- Cox 비례위험 분석 (단변량) ---
cat("\n--- 단변량 Cox 분석 시작 ---\n")

# 분석할 변수 목록
univariate_vars <- c("Age", "Gender", "DM", "HTN", "Prior_CVD", "smoker",
                    "Heart_failure", "High_EEa", "Dialysis_vintage_month","Dialysis_vintage_year",
                    "WBC", "Hc_t", "Albumin", "Ca", "P",
                    "Chol", "Triglyceride", "CRP", "iPTH","iPTH_100", "KT_V",
                    "QRS_duration_msec", "QTc_interval_msec",
                    "EF", "LVm", "LVMI", "E_Ea", "BMI", "EKG_Rhythm_avR",
                    "ACEIARB", "CCB", "BB", "Diuretics", "ASA", "Statin", "VitD", "PhosphateBinder")
# 실제 존재하는 변수만 사용
univariate_vars <- intersect(univariate_vars, names(data))

# 분석 수행
univariate_results <- data.frame(Variable = character(), HR = numeric(), Lower_CI = numeric(), Upper_CI = numeric(), p_value = numeric(), p_value_formatted = character(), stringsAsFactors = FALSE)
for (var in univariate_vars) {
  formula <- as.formula(paste("surv_obj ~", var))
  cox_model <- tryCatch(coxph(formula, data = data), error = function(e) NULL) # 에러 처리 추가
  if (!is.null(cox_model)) {
      cox_summary <- summary(cox_model)
      p_val <- cox_summary$coefficients[1, 5]
      p_val_formatted <- ifelse(p_val < 0.001, "P < 0.001", sprintf("P = %.3f", p_val))
      univariate_results <- rbind(univariate_results, data.frame(
        Variable = var,
        HR = exp(cox_summary$coefficients[1, 1]),
        Lower_CI = exp(cox_summary$coefficients[1, 1] - 1.96 * cox_summary$coefficients[1, 3]),
        Upper_CI = exp(cox_summary$coefficients[1, 1] + 1.96 * cox_summary$coefficients[1, 3]),
        p_value = p_val,
        p_value_formatted = p_val_formatted
      ))
  } else {
      warning(paste("Could not fit univariate Cox model for variable:", var))
  }
}

# 결과 정렬 및 출력
univariate_results <- univariate_results[order(univariate_results$p_value), ]
print("단변량 Cox 분석 결과:")
print(univariate_results[, c("Variable", "HR", "Lower_CI", "Upper_CI", "p_value_formatted")])
write.csv(univariate_results, "Univariate_Cox_Analysis_Results.csv", row.names = FALSE)
cat("단변량 Cox 분석 결과가 'Univariate_Cox_Analysis_Results.csv' 파일로 저장되었습니다.\n")

cat("--- 단변량 Cox 분석 완료 ---\n")

# --- Cox 비례위험 분석 (다변량) ---
cat("\n--- 다변량 Cox 분석 시작 ---\n")

# 분석할 변수 목록
multivariate_vars <- c("EKG_Rhythm_avR", "High_EEa", "Heart_failure", "LVMI", "BB", "DM", 
                      "Age", "Gender", "Prior_CVD", "Ca", "P", "ASA", 
                      "Albumin", "BMI", "iPTH_100", "CRP", "smoker", "Dialysis_vintage_year")
# 실제 존재하는 변수만 사용
multivariate_vars <- intersect(multivariate_vars, names(data))                      

# 다변량 모델 적합
multivariate_formula <- as.formula(paste("surv_obj ~", paste(multivariate_vars, collapse = " + ")))
cox_multivariate <- tryCatch(coxph(multivariate_formula, data = data), error = function(e) NULL)

if (!is.null(cox_multivariate)) {
    cox_multivariate_summary <- summary(cox_multivariate)

    # 결과 데이터프레임 생성 (수정)
    conf_interval_data <- cox_multivariate_summary$conf.int
    coefficients_data <- cox_multivariate_summary$coefficients

    # conf.int와 coefficients의 rowname이 일치하는지 확인하고 정렬 (필요시)
    variable_names <- rownames(conf_interval_data)
    if (!identical(variable_names, rownames(coefficients_data))) {
        # 만약 순서나 내용이 다르다면, variable_names 기준으로 coefficients_data 재정렬
        warning("Row names mismatch between conf.int and coefficients. Attempting to align.")
        coefficients_data <- coefficients_data[variable_names, , drop = FALSE] # drop=FALSE 유지하여 행렬 구조 유지
    }

    multivariate_results <- data.frame(
      Variable = variable_names,
      HR = conf_interval_data[, "exp(coef)"], # conf.int에서 직접 HR 사용
      Lower_CI = conf_interval_data[, "lower .95"], # conf.int에서 직접 Lower CI 사용
      Upper_CI = conf_interval_data[, "upper .95"], # conf.int에서 직접 Upper CI 사용
      p_value = coefficients_data[variable_names, "Pr(>|z|)"] # 정렬된 이름으로 p-value 매칭
    )

    # P-value 포맷팅
    multivariate_results$p_value_formatted <- ifelse(multivariate_results$p_value < 0.001, "P < 0.001",
                                                    sprintf("P = %.3f", multivariate_results$p_value))

    # 결과 정렬 및 출력
    multivariate_results <- multivariate_results[order(multivariate_results$p_value), ]
    print("다변량 Cox 분석 결과:")
    print(multivariate_results[, c("Variable", "HR", "Lower_CI", "Upper_CI", "p_value_formatted")])

    # 모델 적합도 평가 출력
    print("모델 적합도 평가:")
    print(paste("  Concordance:", round(cox_multivariate_summary$concordance[1], 3)))
    print(paste("  Likelihood ratio test p-value:", format.pval(cox_multivariate_summary$logtest[3], digits = 3)))
    print(paste("  Wald test p-value:", format.pval(cox_multivariate_summary$waldtest[3], digits = 3)))
    print(paste("  Score (logrank) test p-value:", format.pval(cox_multivariate_summary$sctest[3], digits = 3)))

    # CSV 파일로 저장
    write.csv(multivariate_results, "Multivariate_Cox_Analysis_Results.csv", row.names = FALSE)
    cat("다변량 Cox 분석 결과가 'Multivariate_Cox_Analysis_Results.csv' 파일로 저장되었습니다.\n")
} else {
    warning("Could not fit multivariate Cox model.")
}

cat("--- 다변량 Cox 분석 완료 ---\n")

# --- 모델 비교 분석 (C-index, LRT, NRI/IDI) ---
cat("\n--- 모델 비교 분석 시작 ---\n")

# 기본 모델 변수 정의
base_vars <- c("Age", "Gender", "Dialysis_vintage_year", "DM", "Prior_CVD", "Albumin",
              "Ca", "P", "iPTH_100", "Heart_failure", "LVMI")
base_vars <- intersect(base_vars, names(data)) # 존재하는 변수만 사용

# 모델 정의
cox_base <- coxph(as.formula(paste("surv_obj ~", paste(base_vars, collapse = " + "))), data = data)
cox_ext1 <- coxph(as.formula(paste("surv_obj ~", paste(c(base_vars, "EKG_Rhythm_avR"), collapse = " + "))), data = data) # EKG 추가
cox_ext2 <- coxph(as.formula(paste("surv_obj ~", paste(c(base_vars, "High_EEa"), collapse = " + "))), data = data) # E_Ea 추가
cox_ext3 <- coxph(as.formula(paste("surv_obj ~", paste(c(base_vars, "EKG_Rhythm_avR", "High_EEa"), collapse = " + "))), data = data) # 둘 다 추가

# -- C-index 및 LRT 비교 --
cat("\n--- C-index 및 LRT 비교 ---\n")

# C-index 계산
c_base <- summary(cox_base)$concordance[1]
c_ext1 <- summary(cox_ext1)$concordance[1]
c_ext2 <- summary(cox_ext2)$concordance[1]
c_ext3 <- summary(cox_ext3)$concordance[1]

# Delta C-index 계산
delta_c_ext1 <- c_ext1 - c_base
delta_c_ext2 <- c_ext2 - c_base
delta_c_ext3 <- c_ext3 - c_base

# 부트스트랩 함수 정의
bootstrap_ci <- function(model, data, n_boot, time_var, event_var) {
  n <- nrow(data)
  c_indices <- numeric(n_boot)
  model_terms <- terms(model)
  predictor_vars <- all.vars(delete.response(model_terms))
  response_vars <- c(time_var, event_var)
  required_vars <- unique(c(predictor_vars, response_vars))
  
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) { stop(paste("ERROR: Missing required variables:", paste(missing_vars, collapse=", "))) }

  cat(sprintf("Bootstrapping C-index for model (%s) with %d reps... ", deparse(substitute(model)), n_boot))
  pb <- txtProgressBar(min = 0, max = n_boot, style = 3) # 진행 바 추가

  for(i in 1:n_boot) {
    setTxtProgressBar(pb, i) # 진행 바 업데이트
    boot_idx <- sample(1:n, n, replace = TRUE)
    boot_data <- data[boot_idx, required_vars, drop = FALSE]
    
    time_var_name <- time_var
    event_var_name <- event_var

    if (!is.numeric(boot_data[[time_var_name]])) { c_indices[i] <- NA; next }
    if (!is.numeric(boot_data[[event_var_name]])) { c_indices[i] <- NA; next }

    boot_surv_obj <- tryCatch(Surv(time = boot_data[[time_var_name]], event = boot_data[[event_var_name]]), error = function(e) NULL)
    if (is.null(boot_surv_obj)) { c_indices[i] <- NA; next }

    rhs_formula <- paste(predictor_vars, collapse = " + ")
    boot_formula <- as.formula(paste("boot_surv_obj ~", rhs_formula))
    boot_model <- tryCatch(coxph(boot_formula, data = boot_data, ties = "efron"), error = function(e) NULL)

    if (!is.null(boot_model)) {
        conc <- tryCatch(summary(boot_model)$concordance[1], error = function(e) NULL)
        if (!is.null(conc) && !is.na(conc) && is.numeric(conc)) { c_indices[i] <- conc } 
        else { c_indices[i] <- NA }
    } else { c_indices[i] <- NA }
  } 
  close(pb) # 진행 바 닫기

  valid_c_indices <- na.omit(c_indices)
  ci <- if (length(valid_c_indices) >= 2) quantile(valid_c_indices, c(0.025, 0.975), na.rm = TRUE) else c(NA, NA)
  cat(sprintf(" Done. Valid iterations: %d/%d. CI: [%.3f, %.3f]\n", length(valid_c_indices), n_boot, ci[1], ci[2]))
  return(list(ci = ci))
}

# 부트스트랩 실행 (오류 발생 시 NA 반환하도록 수정)
set.seed(123)
n_boot <- 1000
boot_base <- tryCatch(bootstrap_ci(cox_base, data, n_boot, "MACE_or_fu_date_month", "MACE"), error = function(e) list(ci=c(NA, NA)))
boot_ext1 <- tryCatch(bootstrap_ci(cox_ext1, data, n_boot, "MACE_or_fu_date_month", "MACE"), error = function(e) list(ci=c(NA, NA)))
boot_ext2 <- tryCatch(bootstrap_ci(cox_ext2, data, n_boot, "MACE_or_fu_date_month", "MACE"), error = function(e) list(ci=c(NA, NA)))
boot_ext3 <- tryCatch(bootstrap_ci(cox_ext3, data, n_boot, "MACE_or_fu_date_month", "MACE"), error = function(e) list(ci=c(NA, NA)))

# LRT 수행
lrt_ext1_vs_base <- anova(cox_base, cox_ext1)
lrt_ext2_vs_base <- anova(cox_base, cox_ext2)
lrt_ext3_vs_base <- anova(cox_base, cox_ext3)

# p-value 포맷팅 함수
format_lrt_p <- function(lrt_result) {
  p_col_names <- c("P(>|Chi|)", "Pr(>Chisq)", "P", "Pr(>|Chi|)")
  p_col_index <- which(tolower(colnames(lrt_result)) %in% tolower(p_col_names))
  if (length(p_col_index) == 0 || nrow(lrt_result) < 2) return("N/A")
  p_val <- lrt_result[2, p_col_index[length(p_col_index)]]
  if (is.na(p_val)) return ("N/A")
  ifelse(p_val < 0.001, "< 0.001", sprintf("%.3f", p_val))
}

# 결과 요약 테이블 생성
performance_results <- data.frame(
  Model = c("Base", "Base + EKG", "Base + E/Ea", "Base + EKG + E/Ea"),
  C_index = round(c(c_base, c_ext1, c_ext2, c_ext3), 3),
  C_index_95CI = c(sprintf("[%.3f, %.3f]", boot_base$ci[1], boot_base$ci[2]),
                   sprintf("[%.3f, %.3f]", boot_ext1$ci[1], boot_ext1$ci[2]),
                   sprintf("[%.3f, %.3f]", boot_ext2$ci[1], boot_ext2$ci[2]),
                   sprintf("[%.3f, %.3f]", boot_ext3$ci[1], boot_ext3$ci[2])),
  Delta_C_index = round(c(0, delta_c_ext1, delta_c_ext2, delta_c_ext3), 3),
  AIC = round(c(AIC(cox_base), AIC(cox_ext1), AIC(cox_ext2), AIC(cox_ext3)), 1),
  P_vs_Base_LRT = c("-", format_lrt_p(lrt_ext1_vs_base), format_lrt_p(lrt_ext2_vs_base), format_lrt_p(lrt_ext3_vs_base))
)

# 결과 출력 및 저장
print("모델 성능 비교 (C-index, LRT):")
print(performance_results)
write.csv(performance_results, "Model_Performance_Comparison_Cindex_LRT.csv", row.names = FALSE)
cat("모델 성능 비교 결과(C-index, LRT)가 'Model_Performance_Comparison_Cindex_LRT.csv' 파일로 저장되었습니다.\n")

# -- NRI / IDI 분석 --
cat("\n--- NRI / IDI 분석 ---\n")

# 분석 설정
t0 <- 60 # 예측 시점 (5년)
npert <- 500 # 부트스트랩 반복 횟수

# 입력 데이터 준비 (결측치 제거)
all_model_vars <- unique(c(all.vars(formula(cox_ext3)), "MACE_or_fu_date_month", "MACE"))
model_data_subset <- data[, intersect(all_model_vars, names(data)), drop = FALSE] # 존재하는 변수만 선택
valid_rows <- complete.cases(model_data_subset)
data_valid <- data[valid_rows,]
indata_valid <- data_valid[, c("MACE_or_fu_date_month", "MACE")]
cat(sprintf("NRI/IDI 분석에 포함된 대상자 수 (결측치 제거 후): %d
", nrow(data_valid)))

# 공변량 행렬 생성 함수
create_cov_matrix <- function(model, data) {
  model_terms <- terms(model)
  predictor_vars <- all.vars(delete.response(model_terms))
  formula_str <- paste("~", paste(predictor_vars, collapse="+"))
  mm <- model.matrix(as.formula(formula_str), data=data)
  intercept_col <- which(colnames(mm) == "(Intercept)")
  if (length(intercept_col) > 0) { mm <- mm[, -intercept_col, drop = FALSE] }
  return(mm)
}

# 공변량 행렬 생성 (오류 처리 추가)
covs_base <- tryCatch(create_cov_matrix(cox_base, data_valid), error=function(e) {warning("Base 공변량 행렬 생성 실패"); NULL})
covs_ext1 <- tryCatch(create_cov_matrix(cox_ext1, data_valid), error=function(e) {warning("Ext1 공변량 행렬 생성 실패"); NULL})
covs_ext2 <- tryCatch(create_cov_matrix(cox_ext2, data_valid), error=function(e) {warning("Ext2 공변량 행렬 생성 실패"); NULL})
covs_ext3 <- tryCatch(create_cov_matrix(cox_ext3, data_valid), error=function(e) {warning("Ext3 공변량 행렬 생성 실패"); NULL})

# NRI/IDI 계산 실행 (공변량 행렬이 유효한 경우만)
if (all(!sapply(list(covs_base, covs_ext1, covs_ext2, covs_ext3), is.null))) {
    cat(sprintf("IDI & Continuous NRI 계산 중 (t0=%d, npert=%d)...
", t0, npert))
    
    # IDI.INF 호출 함수 (에러 처리 포함)
    run_idi_inf <- function(cov0, cov1, model_name) {
        cat(sprintf("  %s vs Base 실행 중...
", model_name))
        tryCatch({
            IDI.INF(indata=indata_valid, covs0=cov0, covs1=cov1, t0=t0, npert=npert)
        }, error = function(e) {
            warning(paste("IDI.INF failed for", model_name, "vs Base:", e$message)); NULL
        })
    }
    
    nri_ext1 <- run_idi_inf(covs_base, covs_ext1, "Ext 1 (+EKG)")
    nri_ext2 <- run_idi_inf(covs_base, covs_ext2, "Ext 2 (+E/Ea)")
    nri_ext3 <- run_idi_inf(covs_base, covs_ext3, "Ext 3 (+Both)")

    # 결과 추출 및 포맷팅 함수
    extract_nri_idi <- function(idi_inf_result, model_name) {
        if(is.null(idi_inf_result)) {
            return(data.frame(Comparison=model_name, IDI="NA (실패)", NRI_Continuous="NA (실패)", stringsAsFactors = FALSE))
        }
        
        format_metric <- function(metric_name) {
            res <- idi_inf_result$est
            if (is.null(res) || !(metric_name %in% rownames(res))) {
                # $est 가 없거나 metric이 없을 경우 $m1(IDI), $m3(NRI) 확인
                if (metric_name == "IDI" && !is.null(idi_inf_result$m1)) {
                    est <- idi_inf_result$m1[1]; lower <- idi_inf_result$m1[2]; upper <- idi_inf_result$m1[3]; p <- idi_inf_result$m1[4]
                } else if (metric_name == "NRI(cont)" && !is.null(idi_inf_result$m3)) {
                     est <- idi_inf_result$m3[1]; lower <- idi_inf_result$m3[2]; upper <- idi_inf_result$m3[3]; p <- idi_inf_result$m3[4]
                } else {
                    return("NA (결과 없음)")
                }
            } else {
                 # $est 에서 추출
                 col_est <- "Est."; col_lower <- "Lower"; col_upper <- "Upper"; col_p <- "p.value" # 이름 확인 필요
                 if (!("Lower" %in% colnames(res))) col_lower <- grep("Lower.*CI", colnames(res), ignore.case=TRUE, value=TRUE)[1]
                 if (!("Upper" %in% colnames(res))) col_upper <- grep("Upper.*CI", colnames(res), ignore.case=TRUE, value=TRUE)[1]

                 est <- res[metric_name, col_est]
                 lower <- if (!is.na(col_lower) && col_lower %in% colnames(res)) res[metric_name, col_lower] else NA
                 upper <- if (!is.na(col_upper) && col_upper %in% colnames(res)) res[metric_name, col_upper] else NA
                 p <- if (col_p %in% colnames(res)) res[metric_name, col_p] else NA
            }
            
            ci_str <- ifelse(is.na(lower) || is.na(upper), "[CI N/A]", sprintf("[%.3f, %.3f]", lower, upper))
            p_str <- ifelse(is.na(p), "(p N/A)", ifelse(p < 0.001, "(p<0.001)", sprintf("(p=%.3f)", p)))
            sprintf("%.3f %s %s", est, ci_str, p_str)
        }

        data.frame(
            Comparison = model_name,
            IDI = format_metric("IDI"),
            NRI_Continuous = format_metric("NRI(cont)"), # NRI(cont) 또는 NRI 확인 필요
            stringsAsFactors = FALSE
        )
    }

    # 결과 테이블 생성
    nri_results <- rbind(
        extract_nri_idi(nri_ext1, "Ext 1 (+EKG) vs Base"),
        extract_nri_idi(nri_ext2, "Ext 2 (+E/Ea) vs Base"),
        extract_nri_idi(nri_ext3, "Ext 3 (+Both) vs Base")
    )

    # 결과 출력 및 저장
    cat("
IDI 및 연속형 NRI 결과 (CI 및 p-value 포함):
")
    print(nri_results, row.names = FALSE)
    write.csv(nri_results, "NRI_IDI_Results_CI_pvalue.csv", row.names = FALSE)
    cat("NRI/IDI 결과가 'NRI_IDI_Results_CI_pvalue.csv' 파일로 저장되었습니다.\n")

} else {
    cat("
NRI/IDI 분석을 위한 공변량 행렬 생성에 실패하여 분석을 건너<0xEB><0x9C><0x91>니다.
")
}

cat("--- 모델 비교 분석 완료 ---\n")

cat("
--- 전체 분석 완료 ---\n")

