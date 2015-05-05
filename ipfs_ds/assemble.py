import sys
import os
import json
import base64

def assemble(index, folder, dest):
    ## index = sys.argv[1]
    ## folder = sys.argv[2]
    ## dest = sys.argv[3]

    with open(index) as data_file:    
        data = json.load(data_file)

    blobs = [i["hash"] for i in data["links"]]

    with open(dest, "wb") as outfile:
        for item in blobs:
            path = os.path.join(folder, item)
            with open(path, "r") as source:
                data = json.load(source)
                binary = base64.b64decode(data["data"])
                outfile.write(binary)
