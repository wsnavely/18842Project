#!/usr/bin/env python
import cgi
import os
import datetime
import random
 
print "Content-type: text/html"
print
print "<title>Script Runner</title>"
args = cgi.FieldStorage()
if "action" in args:
    action = args["action"].value
    if action == "get_file":
        fname = args["name"].value
        cmd = ["python", "ipfs.py", "--getfile", fname]
        cmd_str = " ".join(cmd) + " > get_file_output"
        print "<p> Running: " + cmd_str + "</p>"
        os.system(cmd_str)

        cmd_str = "python draw_graph.py | neato -Tpng -o graph.png"
        print "<p> Running: " + cmd_str + "</p>"
        os.system(cmd_str)
    else: 
        print "Unrecognized action"
else:
    print "No action specified"

