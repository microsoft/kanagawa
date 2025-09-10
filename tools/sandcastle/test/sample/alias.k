module sample.alias
  { core
  , proc
  }

template <auto N>
class core
{
public:
  using addr_t = uint8;
}

class proc
{
  using core_t = core<4>;
public:
  using addr_t = core_t::addr_t;
}
