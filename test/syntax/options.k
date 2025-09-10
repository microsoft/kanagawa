// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  import .options

  static assert(stall == 3);
  static assert(backend == Backend::Sv);

expected:0

  import .options as opt

  static assert(opt::stall == 3);
  static assert(opt::backend == opt::Backend::Sv);

expected:0

  import .options

  const Backend[2] backends = {
    Backend::None,
    Backend::Sv};

expected:0

  import .options as opt

  inline void main()
  {
      static if (opt::backend == opt::Backend::Sv)
      {
          bool x;
      }
      else
      {
          int<opt::stall> x;
      }
  }

expected:0

  static assert(stall == 13);

expected:1

  static assert(backend == Backend::Sv);

expected:1

  import .options

warning:redundant-import
