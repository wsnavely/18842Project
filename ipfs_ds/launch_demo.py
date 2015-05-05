import os

for client in os.listdir("clients"):
    cmd = ["erl"]
    target = os.path.join("clients", client)
    cmd += ["-noshell", "-sname", "node_" + client, "-run", "ipfs_demo", "main", client, "client.conf", target]
    cmd = " ".join(cmd)
    print cmd
    os.system(cmd + " &")
