# SNMP Appendix C

## Appendix C

### Compile time configuration

There is one compile/configure time option: Defining the size of an "empty" PDU. This is used when processing get-bulk requests. The default value for this is *21*, but can be *increased* in two ways:

* configure: `--with-snmp-empty-pdu-size=SIZE`
* compile time: `environment variable: SNMP_EMPTY_PDU_SIZE=SIZE"`

Where `SIZE` is a value greater or equal to 21.
