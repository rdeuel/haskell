import HttpServer

simple req = "<html><body><h1>Fuck Off<br>" ++ (path req) ++ "</h1></body></html>"

resp page = "HTTP/1.0 200 OK\r\n" ++
       "Date: Fri, 14 Jan 2014 23:59:59 GMT\r\n" ++
       "Content-Type: text/html\r\n" ++
       "Content-Length: " ++ (show $ length page) ++ "\r\n\r\n" ++ page

login = "<h2>Login</h2>\
\<form action=\"/login\" method=\"post\">\
   \<table>\
      \<tr>\
         \<td>Username</td>\
         \<td><input type=\"text\" name=\"username\" value=\"\"></td>\
      \</tr>\
      \<tr>\
         \<td>Password</td>\
         \<td><input type=\"password\" name=\"password\"></td>\
         \<td><font color=\"red\">Something</font></td>\
      \</tr>\
   \</table>\
   \<input type=\"submit\">\
\</form>"

--genResponse = resp . HttpServer.path
genResponse _ = resp login
    
main = HttpServer.start 8080 genResponse


