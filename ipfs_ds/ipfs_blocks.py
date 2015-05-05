import os
import sys
import hashlib
import json
import base64

def chunkify(file_object, chunk_size=1024):
    while True:
        data = file_object.read(chunk_size)
        if not data:
            break
        yield data

def get_block_id(block):
    return hashlib.sha1(block).hexdigest()

def get_file_blocks(path, chunk_size=2**14):
    listing = { "data" : [], "links" : [] }
    count = 0
    result = { "blobs" : {}, "list" : None }
    with open(path, 'rb') as fobj:
        for chunk in chunkify(fobj, chunk_size=chunk_size):
            blob = { "data" : base64.b64encode(chunk) }
            content = json.dumps(blob)
            block_id = get_block_id(content)
            listing["data"].append("blob")
            listing["links"].append({ "hash" : block_id, "size" : len(content) })
            result["blobs"][block_id] = content 

    listing_content = json.dumps(listing)
    listing_id = get_block_id(listing_content)
    result["list"] = (listing_id, listing_content)
    return result
