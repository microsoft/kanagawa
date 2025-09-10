module sample.callbacks
  { callback
  , callback_test
  , callback_class
  , callback_class_2
  , callback_latency
  }

using callback =
    ( /*| Pre-arg 1.*/ uint32 //< Post-arg 1.
    , /*| Pre-arg 2.*/ bool   //< Post-arg 2.
    ) -> uint8;

//| Test callbacks
class callback_test
{
private:
  uint2 default_callback(uint32 x)
  {
      return 0;
  }

  (uint32 x) -> uint2 callback = default_callback;
}

class callback_class
{
    //| doc callback type
    using doc_callback_t = () -> void;
    //| callback with default before decl
    doc_callback_t callback_before = default_callback;
    //| doc callback default
    void default_callback(){}
    //| callback
    doc_callback_t callback = default_callback;

    (uint32 x) -> uint2 callback1;
}

class callback_class_2
{
  callback_class::doc_callback_t priv_callback = callback_class::default_callback;

  //| private callback 2
  callback_class::doc_callback_t priv_callback2 = callback_class::default_callback;
public:
  //| public callback
  callback_class::doc_callback_t pub_callback = callback_class::default_callback;

  class inner_callback_class
  {
    callback_class::doc_callback_t inner_priv_callback  = callback_class::default_callback;
    callback_class::doc_callback_t inner_priv_callback2 = callback_class_2::priv_callback;
    callback_class::doc_callback_t inner_priv_callback3 = callback_class_2::pub_callback;
  public:
    callback_class::doc_callback_t inner_pub_callback = callback_class::default_callback;
    callback_class::doc_callback_t inner_pub_callback2 = callback_class_2::priv_callback;
    callback_class::doc_callback_t inner_pub_callback3 = callback_class_2::pub_callback;
  }
}

//| Latency and callback
class callback_latency
{
private:
  [[latency(2)]]
  (uint32 x) -> uint2 lc_callback;
}
