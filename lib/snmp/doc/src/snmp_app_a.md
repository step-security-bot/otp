# SNMP Appendix A

## Appendix A

This appendix describes the conversion of SNMPv2 to SNMPv1 error messages. The instrumentation functions should return v2 error messages.

Mapping of SNMPv2 error message to SNMPv1:

| SNMPv2 message | SNMPv1 message |
|----------------|----------------|
| noError | noError |
| genErr | genErr |
| noAccess | noSuchName |
| wrongType | badValue |
| wrongLength | badValue |
| wrongEncoding | badValue |
| wrongValue | badValue |
| noCreation | noSuchName |
| inconsistentValue | badValue |
| resourceUnavailable | genErr |
| commitFailed | genErr |
| undoFailed | genErr |
| notWritable | noSuchName |
| inconsistentName | noSuchName |


*Table: Error Messages*
