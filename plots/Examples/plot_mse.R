color <- c("red", "#E69F00", "#009E73", "blue", "purple", "grey")

# logistic regression
plot(rev(mnum), rev(df_mse_mean$mse_opt_mean), ylim = c(3.4e-3, 2e-2), type = "o", 
     col = color[1], pch = 0, log='y', xaxt="n", yaxt="n",
     xlab = "Number of Subsamples", ylab = "Mean Squared Error", main="")
axis(side = 1, at = rev(mnum), labels = rev(mnum))
axis(side = 2, at = c(4e-3, 6e-3, 1e-2, 2e-2), labels = c(4e-3, 6e-3, 1e-2, 2e-2))
lines(rev(mnum), rev(df_mse_mean$mse_aver_mean), type = "o", pch = 1, col = color[2])
lines(rev(mnum), rev(df_mse_mean$mse_reboot_mean), type = "o", pch = 2, col = color[3])
lines(rev(mnum), rev(df_mse_mean$mse_csl1_mean), type = "o", pch = 5, col = color[4])
lines(rev(mnum), rev(df_mse_mean$mse_csl2_mean), type = "o", pch = 6, col = color[5])
lines(rev(mnum)[1:4], rev(df_mse_mean$mse_savgm_mean)[1:4], type = "o", pch = 7, col = color[6])
legend("topleft", inset = 0.01, pch = c(0,1,2,5,6,7), lty = 1, horiz = F, col = color, 
       legend = c("Opt","AVGM", "Reboot", "CSL1", "CSL2","SAVGM"),
       box.lty=0, cex=.9)


plot(rev(mnum), rev(df_mse_mean$mse_opt_mean), ylim = c(7e-3, 1e-1), type = "o", 
     col = color[1], pch = 0, log='y', xaxt="n", yaxt="n",
     xlab = "Number of Subsamples", ylab = "Mean Squared Error", main="")
axis(side = 1, at = rev(mnum), labels = rev(mnum))
axis(side = 2, at = c(1e-2, 2e-2, 5e-2, 1e-1), labels = c(1e-2, 2e-2, 5e-2, 1e-1))
lines(rev(mnum), rev(df_mse_mean$mse_aver_mean), type = "o", pch = 1, col = color[2])
lines(rev(mnum), rev(df_mse_mean$mse_reboot_mean), type = "o", pch = 2, col = color[3])
lines(rev(mnum), rev(df_mse_mean$mse_csl1_mean), type = "o", pch = 5, col = color[4])
lines(rev(mnum), rev(df_mse_mean$mse_csl2_mean), type = "o", pch = 6, col = color[5])
lines(rev(mnum)[1:3], rev(df_mse_mean$mse_savgm_mean)[1:3], type = "o", pch = 7, col = color[6])
legend("topleft", inset = 0.01, pch = c(0,1,2,5,6,7), lty = 1, horiz = F, col = color, 
       legend = c("Opt","AVGM", "Reboot", "CSL1", "CSL2","SAVGM"),
       box.lty=0, cex=.9)
