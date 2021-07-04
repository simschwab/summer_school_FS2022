emailing <- function(to, from, subject, text, password){
   
  library(emayili)
  
    email <- envelope(
    to = to,
    from = from,
    subject = subject,
    text = text
  )
  
  smtp <- server(host = "smtp.gmail.com",
                 port = 465,
                 username = from,
                 password = password)
  smtp(email, verbose = FALSE)  
}