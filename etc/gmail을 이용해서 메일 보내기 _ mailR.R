
# https://support.google.com/accounts/answer/185833?hl=en#
# https://medium.com/airbnb-engineering/using-googlesheets-and-mailr-packages-in-r-to-automate-reporting-c09579e0377f

library(mailR)
# Write the content of your email
msg <- paste("Hi team,","","The mpg car dashboard is up-to-date as of",as.character(date()),
             "and can be accessed here: https://docs.google.com/spreadsheets/d/1LYxV8Z324o-OALSwO2h99fUecBdb_t-8BYgBbe0G1KU/edit#gid=0","",
             "Best,","Your name")

# Define who the sender is
sender <- "keivntemp007@gmail.com"
# Define who should get your email
recipients <- c("kevin.ko@agilesoda.com")
              
# Send your email with the send.mail function
send.mail(from = sender,
        to = recipients,
        subject = "Top 10 cars dashboard",
        body = msg,
        smtp = list(host.name = "smtp.gmail.com", port = 587,
                    user.name = "kevintemp007@gmail.com",
                    passwd = "crpvhgpgjkcwsgqe", ssl = TRUE),
        authenticate = TRUE,
        send = TRUE)



#-----------------------------------------------------------------------------------
set.seed(1)
info <- data.frame(process = sample(c("A","B"), 6, replace=T),
                   value = runif(6))
info

recieve <- data.frame(process = sample(c("A","B"), 4, replace=T),
                      to = paste0(sample(letters,4),"@gmail.com"))
recieve


process = unique(info$process)
process

library(dplyr)
for (pro in process) {
    # pro = "A"
    
    recipients <- recieve %>% filter(process == pro) %>% select(to) %>% 
        apply(2,as.character) %>% as.character()
    recipients            
    
    msg <- info %>% filter(process == pro) %>% 
        select(value) %>% as.matrix() %>%  
        paste(collapse = "\n")
    msg
    
    # Send your email with the send.mail function
    send.mail(from = "keivntemp007@gmail.com",
              to = recipients,
              subject = "Top 10 cars dashboard",
              body = msg,
              smtp = list(host.name = "smtp.gmail.com", port = 587,
                          user.name = "kevintemp007@gmail.com",
                          passwd = "crpvhgpgjkcwsgqe", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    
}
