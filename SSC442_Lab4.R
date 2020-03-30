library(kernlab)
data("spam")
tibble::as.tibble(spam)
is.factor(spam$type)
levels(spam$type)
set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]
fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)
# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)
?cv.glm

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
cv.glm(spam_trn, fit_over, K = 5)$delta[1]


set.seed(2)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
cv.glm(spam_trn, fit_over, K = 100)$delta[1]


spam_trn_pred = ifelse(predict(fit_caps, spam_trn) > 0,
                       "spam",
                       "nonspam")
spam_trn_pred = ifelse(predict(fit_caps, spam_trn, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_trn_pred2 = ifelse(predict(fit_selected, spam_trn) > 0,
                       "spam",
                       "nonspam")
spam_trn_pred2 = ifelse(predict(fit_selected, spam_trn, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_trn_pred3 = ifelse(predict(fit_additive, spam_trn) > 0,
                       "spam",
                       "nonspam")
spam_trn_pred3 = ifelse(predict(fit_additive, spam_trn, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_trn_pred4 = ifelse(predict(fit_over, spam_trn) > 0,
                       "spam",
                       "nonspam")
spam_trn_pred4 = ifelse(predict(fit_over, spam_trn, type = "response") > 0.5,
                       "spam",
                       "nonspam")

conf_mat_caps = make_conf_mat(predicted = spam_trn_pred, actual = spam_trn$type)
table(spam_trn$type) / nrow(spam_trn_pred)
conf_mat_selected = make_conf_mat(predicted = spam_trn_pred2, actual = spam_trn$type)
table(spam_trn$type) / nrow(spam_trn_pred2)
conf_mat_additive = make_conf_mat(predicted = spam_trn_pred3, actual = spam_trn$type)
table(spam_trn$type) / nrow(spam_trn_pred3)
conf_mat_over = make_conf_mat(predicted = spam_trn_pred4, actual = spam_trn$type)
table(spam_trn$type) / nrow(spam_trn_pred4)
 