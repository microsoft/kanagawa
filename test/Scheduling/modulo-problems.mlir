// RUN: circt-opt %s -ssp-roundtrip=verify
// RUN: circt-opt %s -ssp-schedule=scheduler=simplex | FileCheck %s -check-prefixes=CHECK,SIMPLEX

// CHECK-LABEL: canis14_fig2
// SIMPLEX-SAME: [II<4>]
ssp.instance @canis14_fig2 of "ModuloProblem" [II<3>] {
  library {
    operator_type @L1_1 [latency<1>]
    operator_type @_1 [latency<1>]
  }
  resource {
    resource_type @L1_rsrc [limit<1>]
  }
  graph {
    %0 = operation<@L1_1>(@op3 [dist<1>]) uses[@L1_rsrc] [t<2>]
    %1 = operation<@L1_1>() uses[@L1_rsrc] [t<0>]
    %2 = operation<@_1>(%0, %1) [t<3>]
    %3 = operation<@L1_1> @op3(%2) uses[@L1_rsrc] [t<4>]
    operation<@_1> @last(%3) [t<5>]
  }
}

// CHECK-LABEL: minII_feasible
// SIMPLEX-SAME: [II<3>]
ssp.instance @minII_feasible of "ModuloProblem" [II<3>] {
  library {
    operator_type @_0 [latency<0>]
    operator_type @_1 [latency<1>]
    operator_type @_2 [latency<2>]
    operator_type @L1_3 [latency<3>]
  }
  resource {
    resource_type @L1_rsrc [limit<1>]
  }
  graph {
    %0 = operation<@_0>() [t<0>]
    %1 = operation<@_2>(@op6 [dist<5>]) [t<0>]
    %2 = operation<@_2>(@op5 [dist<3>]) [t<1>]
    %3 = operation<@_1>(%1, %0) [t<2>]
    %4 = operation<@L1_3>(%3, %2) uses[@L1_rsrc] [t<3>]
    %5 = operation<@L1_3> @op5(%0, %4) uses[@L1_rsrc] [t<7>]
    %6 = operation<@L1_3> @op6(%4, %5) uses[@L1_rsrc] [t<11>]
    // SIMPLEX: @last(%{{.*}}) [t<14>]
    operation<@_1> @last(%6) [t<14>]
  }
}

// CHECK-LABEL: minII_infeasible
// SIMPLEX-SAME: [II<4>]
ssp.instance @minII_infeasible of "ModuloProblem" [II<4>] {
  library {
    operator_type @_1 [latency<1>]
    operator_type @L2_1 [latency<1>, limit<2>]
  }
  resource {
    resource_type @L2_rsrc [limit<2>]
  }
  graph {
    %0 = operation<@_1>() [t<0>]
    %1 = operation<@_1>(%0, @op5 [dist<1>]) [t<1>]
    %2 = operation<@L2_1>(%1) uses[@L2_rsrc] [t<2>]
    %3 = operation<@L2_1>(%1) uses[@L2_rsrc] [t<3>]
    %4 = operation<@L2_1>(%1) uses[@L2_rsrc] [t<2>]
    %5 = operation<@_1> @op5(%2, %3, %4) [t<4>]
    // SIMPLEX: @last(%{{.*}}) [t<5>]
    operation<@_1> @last(%5) [t<5>]
  }
}

// CHECK-LABEL: four_read_pipeline
// SIMPLEX-SAME: [II<4>]
ssp.instance @four_read_pipeline of "ModuloProblem" [II<4>] {
  library {
    operator_type @_1 [latency<1>]
    operator_type @L1_1 [latency<1>]
  }
  resource {
    resource_type @L1_rsrc [limit<1>]
  }
  graph {
    %0 = operation<@_1>() [t<0>]
    %1 = operation<@_1>(%0) [t<1>]
    %2 = operation<@L1_1>(%1) uses[@L1_rsrc] [t<2>]
    %3 = operation<@L1_1>(%1) uses[@L1_rsrc] [t<3>]
    %4 = operation<@L1_1>(%1) uses[@L1_rsrc] [t<4>]
    %5 = operation<@L1_1>(%1) uses[@L1_rsrc] [t<5>]
    %6 = operation<@_1>(%2, %3) [t<4>]
    %7 = operation<@_1>(%4, %5) [t<6>]
    %8 = operation<@_1>(%6, %7) [t<7>]
    // SIMPLEX: @last(%{{.*}}) [t<8>]
    operation<@_1> @last(%8) [t<8>]
  }
}
