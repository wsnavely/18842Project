from Crypto.PublicKey import RSA
import sys
import json
import hashlib
import os
import ipfs_blocks 

def gen_id():
    prv_key = RSA.generate(2048)
    pub_key = prv_key.publickey()
    der_prv = prv_key.exportKey(format="DER")
    der_pub = pub_key.exportKey(format="DER")
    hex_prv = der_prv.encode('hex')
    hex_pub = der_pub.encode('hex')
    identity = hashlib.sha1(hex_pub).hexdigest()
    return (identity, hex_prv, hex_pub)

obj = "lecture1.mp4"
blks = ipfs_blocks.get_file_blocks(obj, chunk_size=2**16)
count = 10
k = 2

nodes = {}
for _ in xrange(count):
    node_config = {}
    node_id, prv, pub = gen_id()
    node_config["host"] = "ubuntu-amd64"
    node_config["id"] =  node_id
#    node_config["pub"] = prv
#    node_config["prv"] = pub
    node_config["blocks"] = []
    nodes[node_id] = node_config

ids = [int(conf["id"], 16) for conf in nodes.values()]
for blob_id in blks["blobs"]:
    bid = int(blob_id, 16)
    dist = [(i, bid ^ i) for i in ids]
    dist.sort(key=lambda x:x[1])
    for (node, d) in dist[:k]:
        nid = format(node, '040x')
        nodes[nid]["blocks"].append(blob_id)

try:
    os.system("rm -rf clients")
    os.system("rm -rf files")
    os.mkdir("clients") 
    os.mkdir("files")
except:
    pass

for node_id in nodes:
    node = nodes[node_id]
    client_dir = "clients/" + node["id"]
    blocks_dir = client_dir + "/blocks"
    try:
        os.mkdir(client_dir) 
    except:
        pass
    try:
        os.mkdir(blocks_dir) 
    except:
        pass

    for block in node["blocks"]:
        with open(blocks_dir + "/" + block, 'w') as fobj:
            fobj.write(blks["blobs"][block])

(lid, content) = blks["list"]
outfile = "files/lecture.blocks" 
with open(outfile, 'w') as fobj:
    fobj.write(content)

client_config = { "nodes" : nodes.values() }
outfile = "client.conf"
with open(outfile, 'w') as fobj:
    json.dump(client_config, fobj, sort_keys=True, indent=4, separators=(',', ': '))
