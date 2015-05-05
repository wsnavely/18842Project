import collections

graph = ""
graph += "digraph G {\n"
count = collections.defaultdict(int)
for line in open("get_file_output"):
    line = line.strip()
    items = line.split()
    if len(items) == 4:
        oid = items[1]
        src = items[3]
        count[src] += 1

idx = 1
for item in count:
    name = "N" + str(idx)
    graph += "\t\"" + name + "\" -> client [len=\"1.5\", label=\"" + str(count[item]) + "\"];\n"
    idx += 1
graph += "}\n"

print graph
