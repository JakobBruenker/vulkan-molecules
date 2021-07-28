#define RIO_DATA_FIELD(rdf_parent, rdf_type, rdf_lens, rdf_field) \
  ;rdf_lens :: Lens' env rdf_type; \
  rdf_lens = rdf_parent . lens rdf_field \x y -> x {width = y};
#define RIO_BASIC_CLASS(rbc_super, rbc_class, rbc_name, rbc_lens) \
  class rbc_super => rbc_class env where { \
    rbc_lens :: Lens' env rbc_name }; \
  instance rbc_class rbc_name where { \
    rbc_lens = id }
#define RIO_BASIC_INSTANCE(rbi_class, rbi_name, rbi_lens, rbi_field) \
  instance rbi_class rbi_name where \
    rbi_lens = lens rbi_field \x y -> x {rbi_field = y}
#define RIO_TRANS_INSTANCE(rti_class, rti_name, rti_lens, rti_pre) \
  instance rti_class rti_name where \
    rti_lens = rti_pre . rti_lens
