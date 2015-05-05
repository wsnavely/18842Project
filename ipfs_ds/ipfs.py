from Crypto.PublicKey import RSA
import sys
import json
import hashlib
import os
import ipfs_blocks 
import ipfs_publish
import ipfs_db
import argparse
import requests
import assemble

context_file = "/tmp/ipfs/contexts.json" ##TODO DIRTY

def handle_ls_cmd():
    names = ipfs_db.get_names()

    for name in names["names"]:
        print name

def gen_id():
    prv_key = RSA.generate(2048)
    pub_key = prv_key.publickey()
    exp_prv = prv_key.exportKey()
    exp_pub = pub_key.exportKey()
    identity = hashlib.sha1(exp_pub).hexdigest()
    return (identity, exp_prv, exp_pub)

def gen_client(client_dir, hostname):
    client_config = {}
    cid, prv, pub = gen_id()
    client_config["id"] = cid
    client_config["public_key"] = pub
    client_config["private_key"] = prv
    client_config["hostname"] = hostname

    # Save the client configuration locally
    config = os.path.join(client_dir, ".ipfs")
    with open(config, 'w') as fobj:
        json.dump(client_config, fobj, sort_keys=True, indent=4, separators=(',', ': '))

    # Send myself to the mother brain, without my private key
    cpy = client_config.copy()
    cpy["private_key"] = ""
    ipfs_db.add_node(cpy)

    ## Create client configuration files(called contexts) for locally running nodes.

    return client_config

def load_existing_config(client_dir):
    config = os.path.join(client_dir, ".ipfs")
    with open(config, 'r') as conf:
        return json.load(conf)

def start_ipfs_node(client_config, client_dir):
    cmd = "nohup erl"
    cmd += " -name node_" + client_config["id"] + "@" + client_config["hostname"]
    cmd += " -run ipfs main " + client_dir
    cmd += " -noshell >> out 2>> err"
    print cmd
    os.system(cmd + " &")

def dump_routing_table(client_dir):
    client_config = load_existing_config(client_dir)
    cmd = "erl"
    cmd += " -name temp" + "@" + client_config["hostname"]
    cmd += " -run ipfs dump_routing_table_main " + client_config["id"] + " " + client_config["hostname"]
    cmd += " -run init stop"
    cmd += " -noshell"
    print cmd
    os.system(cmd)

def get_file(client_dir, filename, dest, version = 1):
    ids = json.loads(ipfs_db.get_rootblock(filename))

    if version == 1:
        rootblock = ids["ids"][0]
    else:
        rootblock = ids["ids"][version]

    get_block(client_dir, rootblock)

    if not os.path.isdir(dest):
        os.mkdir(dest)
    client_config = load_existing_config(client_dir)
    cmd = "erl"
    cmd += " -name temp" + "@" + client_config["hostname"]
    cmd += " -run ipfs get_file_main " + " ".join([client_dir + "/blocks/" + 
        rootblock, dest + "/" + filename, client_config["id"], client_config["hostname"], "5"])
    cmd += " -run init stop"
    cmd += " -noshell"
    print cmd
    os.system(cmd)

    print client_dir + "/blocks/" + rootblock
    print client_dir + "/blocks"
    print client_dir + "/" + filename
    assemble.assemble(client_dir + "/blocks/" + rootblock,
                      client_dir + "/blocks",
                      client_dir + "/" + filename)

def get_block(client_dir, bid):
    client_config = load_existing_config(client_dir)
    cmd = "erl"
    cmd += " -name temp" + "@" + client_config["hostname"]
    cmd += " -run ipfs get_block_main " + " ".join([bid, client_dir + "blocks", \
        client_config["id"], client_config["hostname"]])
    cmd += " -run init stop"
    cmd += " -noshell"
    print cmd
    os.system(cmd)

def get_bootstrap_nodes(client_dir):
    nodes = ipfs_db.get_nodes()
    bs_file = os.path.join(client_dir, ".bootstrap")
    with open(bs_file, 'w') as fobj:
        json.dump(nodes, fobj, sort_keys=True, indent=4, separators=(',', ': '))

parser = argparse.ArgumentParser(description="An IPFS client.")
parser.add_argument("-s", "--start", help="Start the IPFS client", action="store_true")
parser.add_argument("-r", "--routingtable", help="Dump the local routing table", action="store_true")
parser.add_argument("-a", "--aws", help="Indicate that we are on an aws node.", action="store_true")
parser.add_argument("-n", "--hostname", help="The hostname to use.", default="127.0.0.1")
parser.add_argument("-l", "--local", help="Indicate that we are running locally", action="store_true")
parser.add_argument("-d", "--dir", help="The directory where the client is stored.", default="local_node")
parser.add_argument("-p", "--publish", help="Publish a file from this node.")
parser.add_argument("-f", "--getfile", help="Download a file from this node.")
parser.add_argument("-b", "--getblock", help="Download a block from this node.")
parser.add_argument("-v", "--version", help="Download this version of the file.")
parser.add_argument("-x", "--list", help="List Files", action="store_true")

args = parser.parse_args()

if args.start:
    client_config = {}
    if os.path.isdir(args.dir):
        print "Loading existing client from:", args.dir
        client_config = load_existing_config(args.dir)
        #write client_dir to file
        f = open(os.getcwd() + '/cgi/client_dir', 'w')
        f.write(args.dir);
        f.close()
    else:
        print "Generating a new IPFS client!"
        if args.aws and args.local:
            print "Stop: If you are on aws, you can't be local. Only amazon can do that."
            exit(1)

        if args.aws:
            url = "http://169.254.169.254/latest/meta-data/public-ipv4"
            rsp = requests.get(url)
            hostname = rsp.text
        else:
            hostname = args.hostname
        print "Using hostname: " + hostname

        client_dir = os.path.abspath(args.dir)
        #write client_dir to file
        f = open(os.getcwd() + '/cgi/client_dir', 'w')
        f.write(client_dir);
        f.close()
        blocks_dir = os.path.join(client_dir, "blocks")
        if not os.path.isdir(client_dir):
            os.mkdir(client_dir)
        os.mkdir(blocks_dir)
        print "Installing client to: " + client_dir 
        get_bootstrap_nodes(client_dir)
        client_config = gen_client(args.dir, hostname)

    print "Starting the IPFS node!"
    start_ipfs_node(client_config, args.dir)
elif args.routingtable:
    dump_routing_table(args.dir)
elif args.publish:
    print "Publishing file " + args.publish + " on node " 
    client_config = load_existing_config(args.dir)
    ipfs_publish.publish_file(args.publish, args.dir, client_config)
elif args.getfile:
    if args.version:
        get_file(args.dir, args.getfile, "fileblocks", int(args.version))
    else:
        get_file(args.dir, args.getfile, "fileblocks")
elif args.getblock:
    get_block(args.dir, args.getblock)
elif args.list:
    handle_ls_cmd()
