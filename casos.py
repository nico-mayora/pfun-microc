import os

N = ["0"+str(i) for i in range(1,10)]+[str(i) for i in range(10,41)]
z = 0

for n in N:
  os.system(f".\\src\\MicroC.exe .\\test\\caso{n}.mc < .\\test\\caso{n}.in > .\\test\\salida{n}.out")
  os.system(f".\\src\\MicroC.exe -o .\\test\\caso{n}.mc < .\\test\\caso{n}.in > .\\test\\salida{n}.opt")
  nt = os.popen(f"diff .\\test\\caso{n}.out .\\test\\salida{n}.out").read()
  no = os.popen(f"diff .\\test\\caso{n}.opt .\\test\\salida{n}.opt").read()
  if (len(nt) > 0):
    z += 1
    print("Error en .out caso",n)
  if (len(no) > 0):
    z += 1
    print("Error en .opt caso",n)

if (z):
  print("Tienes ",z,"errores")
else:
  print("0w0")
