with ProducerPackage; use ProducerPackage;

package ConsumerPackage is

task type Consumer is
	entry start( p : in Producer_ptr; id : in Integer);
end Consumer;
	
type Consumer_ptr is access Consumer;

end ConsumerPackage;
