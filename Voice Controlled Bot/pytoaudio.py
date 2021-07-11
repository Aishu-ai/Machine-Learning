import serial 
from record1 import*

ser=serial.Serial('/dev/ttyACMO',9600)

if(pred==0):
	voice='back'
	ser.write(val.encode("utf-8"))
if(pred==1):
	voice='forward'
	ser.write(val.encode("utf-8"))
if(pred==2):
	voice='left'
	ser.write(val.encode("utf-8"))
if(pred==3):
	voice='right'
	ser.write(val.encode("utf-8"))
if(pred==4):
	voice='stop'
	ser.write(val.encode("utf-8"))
		
		

