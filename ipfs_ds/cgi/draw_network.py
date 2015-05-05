#!/usr/bin/env python

import random
import hashlib
import math
from PIL import Image
import cgi
import json
import boto.dynamodb2
import paramiko
from boto.dynamodb2.table import Table

access_key_id = "REPLACE"
access_key = "REPLACE"
region = "us-east-1"

print "Content-type: text/html"
print
print """
<title>Network Viewer</title>
<head>
</head>
<link rel="stylesheet" href="http://yui.yahooapis.com/pure/0.6.0/pure-min.css">
<body>
<center>
<h2 style="font-family:arial; border-bottom: 8px solid #57c4d0;">The DSFlix P2P Network (Kademlia DHT)</h2>
</center>
"""

conn = boto.dynamodb2.connect_to_region(
    region, 
    aws_access_key_id=access_key_id, 
    aws_secret_access_key=access_key)
nodes = Table("dsflix_nodes", connection=conn)
node_data = []
for item in nodes.scan():
    data = json.loads(item['node_data'])
    node_data.append(data)

key = paramiko.RSAKey.from_private_key_file("ipfs.pem")
for node in node_data:
    try:
        ip = node["hostname"]
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh.connect(ip, pkey=key, username="ubuntu", timeout=5)
        stdin, stdout, stderr = ssh.exec_command("ls /home/ubuntu/dsproject/ipfs_ds/local_node/blocks | wc -l")
        exit_status = stdout.channel.recv_exit_status()
        node["blocks"] = stdout.read().strip()
        ssh.close()
        node["state"] = "ALIVE" 
    except:
        node["blocks"] = "?"
        node["state"] = "DEAD" 

back = Image.open("back.png")
node = Image.open("node.png")
img_w, img_h = back.size
node_w, node_h = node.size
irad = (img_w - 30) / 2.0

circ = int("ff" * 20, 16)
rad = circ / (2 * math.pi)

print """
<script>
function highlight(id) {
    var tbl = document.getElementById("node_table");
    if (tbl != null) {
        for (var i = 1; i < tbl.rows.length; i++) {
            tbl.rows[i].style.backgroundColor = "#FFFFFF";
        }
    }
    var row = document.getElementById(id);
    row.style.backgroundColor = "#9F81F7";
}
</script>
"""

print '<map name="network">'
displace = int(img_w/2.0) - 5
scale = 5
node_data.sort(key=lambda x: int(x["id"],16))
for data in node_data:
    hsh = data["id"]
    val = int(hsh, 16) / float(rad)
    x = int((irad+scale)*math.cos(val) + (displace))
    y = int((irad+scale)*math.sin(val) + (displace))
    back.paste(node, (x,y))
    coords = [x, y, x + node_w, y + node_h]
    coords = ",".join([str(x) for x in coords])
    ip = data["hostname"]
    blocks = data["blocks"]
    fnc = "highlight('" + hsh + "');"
    print '<area shape="rect" href="#" onclick="' + fnc + '" coords="' + coords + '" title="' + str(hsh) + '">'
    scale = scale * -1
print '</map>'
back.save('network.png')

print """
<div style="position: relative; width: 100%;">
<p><img usemap="#network" src="../network.png"></p>
<div style="position: absolute; top: 240px; left: 260px; width: 200px;">
<h1> IPFS </h1>
</div>
<div id="nodeinfo" style="position: absolute; top: 50px; left: 600px; width: 200px;">
<div class="netview">
<table class="pure-table pure-table-horizontal" id="node_table">
  <tr>
    <th>Node</td>
    <th>Ip</td> 
    <th>Blocks</td> 
    <th>State</td> 
  </tr>
"""

for data in node_data:
    print "<tr id=\"" + data["id"] + "\">"
    print "<td>" + data["id"] + "</td>"
    print "<td>" + data["hostname"] + "</td>"
    print "<td>" + data["blocks"] + "</td>"
    print "<td>" + data["state"] + "</td>"
    print "</tr>"

print """
</table>
</div>
</div>
</div>
</body>
"""

print "</body>"
