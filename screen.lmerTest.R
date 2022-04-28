screen.lmerTest <- function (mod,p=NULL) {
  if (is.null(p)) {p <- .05}
  c1 <- as.data.frame(coef(summary(mod))[,4:5])
  dd <- cbind(c1[2:nrow(c1),],as.data.frame(vif.lme(mod)))
  names(dd)[3] <- 'VIF'
  dd$`Pr(>|t|)` <- as.numeric(dd$`Pr(>|t|)`)
  print(dd[dd$`Pr(>|t|)`<p,c(1,3)], digits = 3)}

screen.lmer <- function (mod,stat=NULL) {
  if (is.null(stat)) {stat <- 3}
  c1 <- as.data.frame(coef(summary(mod))[,3])
  dd <- cbind(c1[2:nrow(c1),],as.data.frame(vif.lme(mod)))
  names(dd) <- c('t stat', 'VIF')
  dd$tstat <- as.numeric(dd$`t stat`)
  print(dd %>% filter(abs(tstat) > stat) %>% select(1:2), digits = 3)}
