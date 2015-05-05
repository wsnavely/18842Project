import boto.ec2
import boto.dynamodb2
from boto.dynamodb2.table import Table
import time
import sys
import paramiko
import argparse
import select

parser = argparse.ArgumentParser(description="Sets up an IPFS experiment.")
parser.add_argument("-c", "--count", help="The node count.")
parser.add_argument("-n", "--nodes", nargs='+', help="Existing nodes to configure",)
args = parser.parse_args()

access_key_id = "REPLACE"
access_key = "REPLACE"
region = "us-east-1"

ec2 = boto.ec2.connect_to_region(
    region,
    aws_access_key_id=access_key_id,
    aws_secret_access_key=access_key)

db = boto.dynamodb2.connect_to_region(
    region, 
    aws_access_key_id=access_key_id, 
    aws_secret_access_key=access_key)

if not args.nodes:
    print "Provisioning instances..."
    sg = "sg-7b1f501f"
    ami = "ami-d81415b0"
    sn_id = "subnet-1688913e"
    numnodes = args.count
    reservation = ec2.run_instances(
            ami,
            key_name="ipfs",
            min_count=numnodes,
            max_count=numnodes,
            subnet_id=sn_id,
            instance_type="t2.micro",
            security_group_ids=[sg])

    print "Waiting for instances to launch..."
    done = False
    while not done:
        done = True
        for instance in reservation.instances:
            instance.update()
            if instance.state != 'running':
                done = False
                break
        time.sleep(5)        
    print "All instances started! IP Addresses are:"
    ips = [inst.ip_address for inst in reservation.instances]
    for ip in ips:
        print ip

    print "Waiting for instances to be ready..."
    time.sleep(60)
else:
    ips = args.nodes

print "Clearing the boostrap table..."
nodes = Table("dsflix_nodes2", connection=db)
for item in nodes.scan():
    item.delete()

print "Establishing SSH connections"
ssh_clients = []
key = paramiko.RSAKey.from_private_key_file("ipfs.pem")
for ip in ips:
    print "Connecting to", ip
    ssh = paramiko.SSHClient()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    ssh.connect(ip, pkey=key, username="ubuntu")
    ssh_clients.append(ssh)

for client in ssh_clients:
    print "Syncing git repo..."
    stdin, stdout, stderr = client.exec_command("rm -rf /home/ubuntu/dsproject/;cd /home/ubuntu; git clone -b aws_blocks git@github.com:wsnavely/dsproject.git")
    exit_status = stdout.channel.recv_exit_status()
    print "Building..."
    stdin, stdout, stderr = client.exec_command("cd /home/ubuntu/dsproject/ipfs_ds;make")
    exit_status = stdout.channel.recv_exit_status()
    print "Launching IPFS client..."
    stdin, stdout, stderr = client.exec_command("cd /home/ubuntu/dsproject/ipfs_ds;./clean.sh;python ipfs.py --aws --start")
    exit_status = stdout.channel.recv_exit_status()
    time.sleep(10)

for client in ssh_clients:
    print "Dumping routing table"
    stdin, stdout, stderr = client.exec_command("cd /home/ubuntu/dsproject/ipfs_ds;python ipfs.py --routingtable")
    exit_status = stdout.channel.recv_exit_status()
    print stdout.read()

print "Closing SSH Connections"
for client in ssh_clients:
    client.close()
