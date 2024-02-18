approximate_mode <- function(samples) {
  # calculate the MAP for each participant
  density_obj <- density(samples)
  highest_density_index <- which.max(density_obj$y)
  mode_estimate <- density_obj$x[highest_density_index]
  return(mode_estimate)
}

tag_malingerers <- function(samples, group_estimates) {
  # add group variable indicating probability of malingerer
  samples <- samples |> mutate(group = group_estimates[subj_num])
}

calculate_corr <- function(data){
  # input long data format
   newdata <- data|> group_by(user_id,user_name,organization_name) |>
    summarise(score = sum(acc,na.rm = TRUE))  |> ungroup() |>
    inner_join(Examscore,by=c("user_name"="姓名","organization_name"="学校"))|>
    select(c("score","语文","数学","外语","物理","化学","生物","政治","历史","地理"))
  result_matrix <- cor(newdata,use = "pairwise.complete.obs")
  return(result_matrix[1,c(2:ncol(result_matrix))])
}




potential <- function(data) {
  if (any(is.na(data))) {
    stop("Dataframe contains NA values.")
  }
  quantile85 <- quantile(data$预测赋分,0.85)
  quantile50 <- quantile(data$预测赋分,0.5,)
  quantile15 <- quantile(data$预测赋分,0.15,na.rm=TRUE)
  
  data %>%
    mutate(潜力分类 = case_when(预测赋分 >= quantile85 & (预测赋分/实际赋分) >= 0.98 & (预测赋分/实际赋分) <= 1.02 ~"总体优秀",
                            预测赋分 >= quantile85 & (预测赋分/实际赋分) > 1.02 ~"潜力巨大",
                            预测赋分 >= quantile85 & (预测赋分/实际赋分) < 0.98 ~"加强后劲",
                            预测赋分 >= quantile50 & 预测赋分 < quantile85 & (预测赋分/实际赋分) >= 0.98 & (预测赋分/实际赋分) <= 1.02 ~"总体良好",
                            预测赋分 >= quantile50 & 预测赋分 < quantile85 & (预测赋分/实际赋分) > 1.02 ~"潜力较大",
                            预测赋分 >= quantile50 & 预测赋分 < quantile85 & (预测赋分/实际赋分) < 0.98 ~"后劲较弱",
                            预测赋分 > quantile15 & 预测赋分 < quantile50 & (预测赋分/实际赋分) >= 1~"总体一般",
                            预测赋分 > quantile15 & 预测赋分 < quantile50 & (预测赋分/实际赋分) < 1~"后劲不足",
                            预测赋分 <= quantile15 ~"亟待提升",
                            TRUE ~ NA_character_)) |>
    mutate(潜力分类 = factor(潜力分类, levels = c("总体优秀", "潜力巨大", "加强后劲","总体良好", "潜力较大", "后劲较弱", "总体一般","后劲不足", "亟待提升")))
}

# 潜力分数>=85%
# 
# 总体优秀：潜力分数/实际分数 <= 1.05 & 潜力分数/实际分数 >=0.8
# 【潜力高，和实际分数接近】
# 
# 潜力巨大：潜力分数/实际分数 > 1.05
# 
# 加强后劲：潜力分数/实际分数 <  0.8
# 
# 
# 潜力分数在50-85 之间
# 
# 总体良好：潜力分数/实际分数 < 1.05 & 潜力分数/实际分数 >0.8
# 
# 潜力较大：潜力分数/实际分数 > 1.05
# 
# 后劲较弱：潜力分数/实际分数 < 0.8
# 
# 
# 潜力分数在15-50
# 
# 总体一般：潜力分数/实际分数>=1
# 
# 后劲不足：潜力分数/实际分数<1
# 
# 
# 潜力分数在15 以下
# 亟待提升



predict_subscore <- function(sub_score,subs,predictors) {
  # subs is a string containing all the subjects,
  # e.g. subs <- c("语文", "数学", "英语", "物理", "化学", "生物", "政治", "历史", "地理")
  # predictors is a string containing all the predictors,
  # 预测成绩是用原始分
  # 预测成绩分为两个部分，模型预测成绩部分和现有成绩部分，两者比重按照lm中可解释变异来分配
  for (sub in subs){
    formula_string <- as.formula(paste(sub,"~",paste("`", predictors, "`", sep = "", collapse = " + ")))
    model <- lm(formula_string, data = sub_score)
    r_squared <- summary(model)$r.squared
  
    ceiling_score <- sub_score |> select(all_of(sub)) |> max(na.rm = TRUE)  # 该科目的最高分
    sub_score <- sub_score |> ungroup() |>
      mutate(prediction = predict(model, newdata = sub_score)) |> 
      mutate(prediction = case_when(prediction < 0 ~ 0,
                                   prediction > ceiling_score ~ ceiling_score,
                                    TRUE ~  prediction))  |>   # 将预测值设定在合理范围
      mutate(combined_score = prediction * r_squared + !!sym(sub) * (1 - r_squared)) |>
      mutate(!!sym(paste(sub,"_预测",sep="")) := combined_score) |> 
      select(-prediction)
    
  }
  return(sub_score)
}


library(purrr)
assign_score <- function(percentile_rank) {
  if (percentile_rank <= 0.01) {
    return(100)
  } else if (percentile_rank <= 0.03 ) {
    return(97)
  } else if (percentile_rank <= 0.06) {
    return(94)
  } else if (percentile_rank <= 0.1) {
    return(91)
  } else if (percentile_rank <= 0.15) {
    return(88)
  } else if (percentile_rank <= 0.22) {
    return(85)
  } else if (percentile_rank <= 0.3) {
    return(82)
  } else if (percentile_rank <= 0.39) {
    return(79)
  } else if (percentile_rank <= 0.47) {
    return(76)
  } else if (percentile_rank <= 0.55) {
    return(73)
  } else if (percentile_rank <= 0.62) {
    return(70)
  } else if (percentile_rank <= 0.68) {
    return(67)
  } else if (percentile_rank <= 0.74) {
    return(64)
  } else if (percentile_rank <= 0.8) {
    return(61)
  } else if (percentile_rank <= 0.85) {
    return(58)
  } else if (percentile_rank <= 0.89) {
    return(55)
  } else if (percentile_rank <= 0.93) {
    return(52)
  } else if (percentile_rank <= 0.96) {
    return(49)
  } else if (percentile_rank <= 0.98) {
    return(46)
  } else if (percentile_rank <= 0.99) {
    return(43)
  }
  else  {
    return(40) # 修改为最低的赋分分数
  }
}

calculate_percentile_and_assign_score <- function(scores) {
  # Calculate ranks with NAs kept in place
  rank <- rank(-scores, ties.method = "min", na.last = "keep")
  
  # Calculate percentile ranks
  percentile_rank <- rank / max(rank, na.rm = TRUE)
  
  # Use map_dbl, but handle NA values explicitly
  assigned_scores <- map_dbl(percentile_rank, ~ ifelse(is.na(.x), NA, assign_score(.x)))
  
  return(assigned_scores)
}


# 应用赋分函数
# assignedscore <- rawscore %>%
#   mutate(across(starts_with("物理") | starts_with("化学") | starts_with("生物")| starts_with("政治")| starts_with("历史")| starts_with("地理"),
#                 calculate_percentile_and_assign_score))



create_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Variable options
# vlabels: variable labels
# vlcex: controls the font size of variable labels
# Polygon options:
#   pcol: line color
# pfcol: fill color
# plwd: line width
# plty: line types. Can be a numeric vector 1:6 or a character vector c(“solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”). To remove the line, use plty = 0 or plty = “blank”.
# Grid options:
#   cglcol: line color
# cglty: line type
# cglwd: line width
# Axis options:
#   axislabcol: color of axis label and numbers. Default is “blue”.
# caxislabels: Character vector to be used as labels on the center axis.