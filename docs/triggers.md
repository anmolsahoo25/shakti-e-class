# Triggers

The E-class currently supports the following __type__ of triggers defined in the latest __0.14 draft __of the debug-spec.

* ```Match Control```: 
    1. For this type, the timing is hardwired to 0, meaning all events will be triggered before the execution of the instruction. 
    2. The hit field is not implemented as of now. Future requirements could lead to integration of this feature. 
    3. Maskmax is also hardwired to 0.
    4. Chaining is supported.
    5. The match field can only take the following value: 0, 2 and 3.

* ```Interrupt Triggers```
* ```Exception Triggers```

The reset value of tdata1 registers is ```0x0```. Thus to initialize a trigger a valid type value corresponding to the above should also be set. 

```Trigger Control``` optional register is not implemented as of now. Future requirements could lead to integration of this feature. 

The number of triggers can be changed at compile-time by changing the ```TRIGGERS``` field in soc_config.inc

