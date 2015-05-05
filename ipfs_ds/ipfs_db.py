import json
import boto.dynamodb2
from boto.dynamodb2.table import Table

access_key_id = "REPLACE"
access_key = "REPLACE"
region = "us-east-1"

conn = boto.dynamodb2.connect_to_region(
    region,
    aws_access_key_id=access_key_id,
    aws_secret_access_key=access_key)

def clear_nodes():
#    pass
    nodes = Table("dsflix_nodes", connection=conn)
    for item in nodes.scan():
        item.delete()

def get_nodes():
#    pass
    nodes = Table("dsflix_nodes", connection=conn)
    result = {"nodes" : []}
    for item in nodes.scan():
        data = json.loads(item['node_data'])
        result["nodes"].append(data)
    return result

def get_files():
    files = Table("dsflix_files", connection=conn)
    result = {"files" : []}
    for item in files.scan():
        #data = json.loads(item['file_data'])
        data = item['file_data']
        result["files"].append(data)
    return result

def add_node(client_config):
#    pass
    nodes = Table("dsflix_nodes", connection=conn)
    nodes.put_item(
        data= {
            'node_id': client_config["id"],
            'node_data': json.dumps(client_config)
        })

def get_names():
    nodes = Table("dsflix_files", connection=conn)
    result = {"names" : [], "ids" : []}
    for item in nodes.scan():
        data = item['file_name']
        result["names"].append(data)
        data = item['file_data']
        result["ids"].append(data)
    return result

def get_rootblock(filename):
    nodes = Table("dsflix_files", connection=conn)
    result = {"names" : [], "ids" : []}
    for item in nodes.scan():
        data = item['file_name']
        if data == filename:
            return item["file_data"]
    return False

def clear_names():
    nodes = Table("dsflix_files", connection=conn)
    for item in nodes.scan():
        item.delete()

def publish_name(filename, rootblock):
    files = Table("dsflix_files", connection=conn)
    try:
        updated_rootblock_item = files.get_item(file_name = filename)
        updated_rootblock = json.loads(updated_rootblock_item['file_data'])
    except boto.dynamodb2.exceptions.ItemNotFound as e:
        updated_rootblock = {"ids" : []}
        print "This is a new file"

    if rootblock in updated_rootblock["ids"]:
        print "This file is already present in the repository."
        return -1

    updated_rootblock["ids"].append(rootblock)

    files.put_item(
        data = {
            'file_name': filename,
            'file_data': json.dumps(updated_rootblock)
        }, overwrite=True)

    return 1
