


df$Response_Group <- ifelse(df$Treatment_HbA1c <= 58 & df$Diff_HbA1c < 0, "Yes", "No")

