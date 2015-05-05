#!/usr/bin/python
#coding=utf-8

import os
import sys
import cgi
sys.path.insert(0, os.path.dirname(os.getcwd()))
import ipfs_db
import ipfs

def get_client_dir():
    with open(os.getcwd() + '/client_dir') as f:
        client_dir = f.readline()
    return str(client_dir) + "/"

client_dir = get_client_dir()

ipfs_path = "/ipfs_ds/"

ipfs_nodes = ipfs_db.get_nodes()

args = cgi.FieldStorage()

selected_node = str(args.getvalue('ip'))

file_name = str(args.getvalue('filename'))

mode = str(args.getvalue('mode'))

if mode == 'None':
    pass
elif mode == 'download':
    ipfs.get_file(client_dir, file_name, "fileblocks")
else:
    full_path = client_dir + file_name
    if sys.platform.lower() == 'darwin':
        os.system('open ' + full_path) # for MAC OS
    elif sys.platform.lower() == 'windows':
        os.system('start ' + full_path) # for windows

print "Content-type:text/html\n\n"
print '<!DOCTYPE html>'
print '<html lang="en">'
print '  <head>'
print '    <meta charset="utf-8">'
print '    <meta http-equiv="X-UA-Compatible" content="IE=edge">'
print '    <meta name="viewport" content="width=device-width, initial-scale=1">'
print '    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->'
print '    <meta name="description" content="File Browser of IPFS">'
print '    <meta name="author" content="Xinkai Wang">'
print '    <link rel="icon" href="' + ipfs_path + 'dsflix/img/favicon.ico">'
print ''
print '    <title>IPFS File Browser</title>'
print ''
print '    <!-- Bootstrap core CSS -->'
print '    <link href="' + ipfs_path + 'dsflix/bs/dist/css/bootstrap.min.css" rel="stylesheet">'
print ''
print '    <!-- Custom styles for this template -->'
print '    <link href="' + ipfs_path + 'dsflix/dashboard.css" rel="stylesheet">'
print ''
print "    <!-- Just for debugging purposes. Don't actually copy these 2 lines! -->"
print '    <!--[if lt IE 9]><script src="../../assets/js/ie8-responsive-file-warning.js"></script><![endif]-->'
print '    <script src="' + ipfs_path + 'dsflix/bs/docs/assets/js/ie-emulation-modes-warning.js"></script>'
print ''
print '    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->'
print '    <!--[if lt IE 9]>'
print '      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>'
print '      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>'
print '    <![endif]-->'
print ''
print '  </head>'
print ''
print '  <body>'
print ''
print '    <nav class="navbar navbar-inverse navbar-fixed-top">'
print '      <div class="container-fluid">'
print '        <div class="navbar-header">'
print '          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">'
print '            <span class="sr-only">Toggle navigation</span>'
print '            <span class="icon-bar"></span>'
print '            <span class="icon-bar"></span>'
print '            <span class="icon-bar"></span>'
print '          </button>'
print '          <a class="navbar-brand" href="#">IPFS File Browser</a>'
print '        </div>'
print '        <div id="navbar" class="navbar-collapse collapse">'
print '          <ul class="nav navbar-nav navbar-right">'
print '            <li><a href="#">Dashboard</a></li>'
print '            <li><a href="#">Log out</a></li>'
print '          </ul>'
print '          <!--<form class="navbar-form navbar-right">'
print '            <input type="text" class="form-control" placeholder="Search...">'
print '          </form> -->'
print '        </div>'
print '      </div>'
print '    </nav>'
print ''

print '    <div class="container-fluid">'
print '      <div class="row">'
print '        <div class="col-sm-3 col-md-2 sidebar">'
print '          <ul class="nav nav-sidebar">'
print '            <li class="active"><a href="' + ipfs_path +  'dsflix/FileBrowser.html">Overview <span class="sr-only">(current)</span></a></li>'
print '          </ul>'
print '          <ul class="nav nav-sidebar">'
for node in ipfs_nodes["nodes"]:
    if node["hostname"] == selected_node:
        print '            <li class="active"><a href="http://localhost/py/show_browser.py?ip=%s">' % node["hostname"]
    else:
        print '            <li><a href="http://localhost/py/show_browser.py?ip=%s">' % node["hostname"]
    print node["hostname"]
    print '</a></li>'
print '          </ul>'
print '        </div>'
print '        <div class="col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main">'
if selected_node == "":
    print '          <h1 class="page-header">Dashboard</h1>'
else:
    print '          <h1 class="page-header">' + selected_node + '</h1>'
print ''
print '          <h2 class="sub-header">Files</h2>'
print '          <div class="table-responsive">'
print '            <table class="table table-striped">'
print '              <thead>'
print '                <tr>'
print '                  <th>ID</th>'
print '                  <th>File Name</th>'
print '                  <th>Download</th>'
print '                </tr>'
print '              </thead>'
print '              <tbody>'
node_files = ipfs_db.get_names()

for i in range(len(node_files['names'])):
    print '                <tr>'
    print '                  <td>'
    print node_files['ids'][i][10:42]
    print '</td>'
    print '                  <td>'
    print node_files['names'][i]
    print '</td>'
    print '                  <td>'
    full_path = client_dir + node_files['names'][i]
    if os.path.isfile(full_path):
        print '<a href="http://localhost/py/show_browser.py?ip=%s&filename=%s&mode=%s">' % (node["hostname"], node_files['names'][i], 'open') + 'Open</a>'
    else:
        print '<a href="http://localhost/py/show_browser.py?ip=%s&filename=%s&mode=%s">' % (node["hostname"], node_files['names'][i], 'download') + 'Download</a>'
    print '</td>'
    print '                </tr>'

print '              </tbody>'
print '            </table>'
print '          </div>'
print '        </div>'
print '      </div>'
print '    </div>'
print ''
print '    <!-- Bootstrap core JavaScript'
print '    ================================================== -->'
print '    <!-- Placed at the end of the document so the pages load faster -->'
print '    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>'
print '    <script src="' + ipfs_path + 'dsflix/dist/js/bootstrap.min.js"></script>'
print "    <!-- Just to make our placeholder images work. Don't actually copy the next line! -->"
print '    <script src="' + ipfs_path + 'dsflix/bs/docs/assets/js/vendor/holder.js"></script>'
print '    <!-- IE10 viewport hack for Surface/desktop Windows 8 bug -->'
print '    <script src="' + ipfs_path + 'dsflix/bs/docs/assets/js/ie10-viewport-bug-workaround.js"></script>'
print '  </body>'
print '</html>'