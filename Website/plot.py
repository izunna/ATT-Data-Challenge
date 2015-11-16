f = open("../national.txt")
o = open("out.csv","w")
for line in f.readlines():
  line = line.strip()
  line = line.split()
  o.write(line[1] + ",")
  o.write(line[2])
  i = 3
  while True:
    try:
      c = int(line[i])
      break
    except ValueError:
      o.write(" " + line[i])
      i+=1

  o.write("," + line[-2] + ",")
  o.write(line[-1] + "\n")  

f.close()
o.close() 
