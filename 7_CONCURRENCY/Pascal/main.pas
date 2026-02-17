-- type declarations --

var 
	myproducer : producer;
	myconsumer : consumer;
	mybuffer : buffer;

begin
	init mybuffer, myproducer(mybuffer), myconsumer(mybuffer);
end;
	
