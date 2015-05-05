#!/usr/bin/python2
import ipfs_blocks
import ipfs_db
import ntpath
import os
import json
import boto.dynamodb

def publish_file_ipfs(metadata_id, client_config):
    cmd = "erl"
    cmd += " -name temp" + "@" + client_config["hostname"]
    cmd += " -run ipfs_helper publish_file_helper "
    cmd += "node_" + client_config["id"]
    cmd += "@" + client_config["hostname"]
    cmd += " " + metadata_id
    cmd += " -run init stop"
    cmd += " -noshell"
    print cmd
    os.system(cmd)

def publish_block_local(homedir, blob_id, block):
    blocks_dir = homedir + "/blocks"
    if not os.path.exists(blocks_dir):
        os.mkdir(blocks_dir)
    try:
        with open(blocks_dir + "/" + blob_id, "w") as fobj:
            fobj.write(block)
    except IOError:
        pass

    with open(homedir + "/.ipfs", "r") as fobj:
        client_config = json.load(fobj)

def publish_file(path, homedir, client_config):
    print "Publishing in " + homedir

    blocks = ipfs_blocks.get_file_blocks(path)
    blobs = blocks["blobs"]
    block_dir = os.path.join(homedir, "blocks")

    (meta_id, metadata) = blocks["list"]

    if ipfs_db.publish_name(os.path.basename(path), meta_id) < 0:
        print "Not publishing."
        return

    for blob_id in blobs:
        block = blobs[blob_id]
        publish_block_local(homedir, blob_id, block)

    publish_block_local(homedir, meta_id, metadata)
    publish_file_ipfs(meta_id, client_config)
