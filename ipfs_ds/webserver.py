import SocketServer
import BaseHTTPServer
import CGIHTTPServer
import cgitb
import os

class ThreadedServer(SocketServer.ThreadingMixIn, BaseHTTPServer.HTTPServer):
    pass

cgitb.enable()
server = ThreadedServer
handler = CGIHTTPServer.CGIHTTPRequestHandler
server_address = ("", 8080)
handler.cgi_directories = ["/cgi"]
httpd = server(server_address, handler)
httpd.serve_forever()
