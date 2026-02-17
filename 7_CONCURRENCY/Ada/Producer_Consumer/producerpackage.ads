package ProducerPackage is

task type Producer is
	entry produce ( v : in Integer);
	entry get( v : out Integer);
end Producer;

type Producer_ptr is access Producer;

end ProducerPackage;
