#ifndef _COMPLIANCE_TEST_H
#define _COMPLIANCE_TEST_H

#define RVMODEL_HALT                                              \
  ecall;                                                          \
  loop: j loop;

#define RVMODEL_DATA_BEGIN                                        \
  .align 4; .global begin_signature; begin_signature:

#define RVMODEL_DATA_END                                          \
  .align 4; .global end_signature; end_signature:

#define RVMODEL_BOOT
#define RVMODEL_SET_MSW_INT
#define RVMODEL_CLEAR_MSW_INT
#define RVMODEL_CLEAR_MTIMER_INT
#define RVMODEL_CLEAR_MEXT_INT

#define RVMODEL_IO_ASSERT_GPR_EQ(a, b, c)

#endif // _COMPLIANCE_TEST_H
