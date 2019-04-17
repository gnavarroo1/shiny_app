
install.packages("RCurl")
library(httr)

# Tranferencia de archivos
system(command = "sshpass -p 'root' scp -o StrictHostKeyChecking=no -r ~/app root@172.17.0.4:/srv/shiny-server/")
system(command = "sshpass -p 'root' scp -o StrictHostKeyChecking=no -r ~/ root@172.17.0.1:/home/rstudio/")

#test correo
body = list(sender = "g.navupc@gmail.com",
            password = "tassadark",
            recipient="u913385@upc.edu.pe",
            subject="test3",
            msg="Hola")
result <- POST("http://35.237.66.122:8000/sendMail",
               body = body,
               encode = "json",verbose())
Output <- content(result)
Output
