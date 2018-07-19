#	array = []

datas = {}


with open("lat-17lon-43/modis_fire_lat-17lon-43.txt", "r") as ins:
	for line in ins:
		#array.append(line)
		e=line.split(" ")
		dia=e[0]
		hora=int(e[1])
		confiabilidade=int(e[9])
		#print("conf",confiabilidade)
		if confiabilidade >=70:
			#print(hora)
			#print(" "+e[1]+"\n")
			if hora>2199 and hora<2399:
				hora='00'
			elif hora>0 and hora<299:
				hora='00'
			elif hora>299 and hora<999:
				hora='06'
			elif hora>999 and hora<1499:
				hora='12'
			elif hora>1499 and hora<2199:
				hora='18'
			else:
				print("Deu ruim")
				break
			key=dia+hora
			if key in datas:
				datas[key]+=1
			else:
				datas[key]=1

		
datas2={}
with open("datas.txt", "r") as ins:
	for line in ins:
		e=line.replace(" ","")
		e=e.replace("\n","")
		#print(e)
		if e in datas:
			datas2[e]=datas[e]
		else:
			datas2[e]=0

values=[]
print("YYYYMMDDHH FOGO")
for key, value in datas2.items():
	print(key,value)
	values.append(value)
#print(len(datas2))
#print(max(values))


		

