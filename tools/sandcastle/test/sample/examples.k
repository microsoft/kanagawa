module sample.examples
    { collapse_example
    , collapse_examples
    , summary_examples
    , nested_example
    , multiple_examples
    }

//| This is a comment
//
// #### __Example__
//
//     >>> collapse_example()
//     "hello"
string collapse_example()
{
    return "hello";
}

//| #### __Examples__
//
//     >>> collapse_examples()
//     "hello"
//
//     >>> collapse_examples()
//     "world"
void collapse_examples(){}

//| Test optional preceding summary.
//
// #### __Examples__
//
//     >>> shift_array_left<4>({0, 1, 2, 3, 4}, 10);
//     {0, 0, 0, 0}
//
// Shift 5 left by 0:
//
//     >>> shift_array_left<5>({0, 1, 2, 3, 4}, 0);
//     {0, 1, 2, 3, 4}
//
// Shift 5 left by 2 and return 4:
//
//     >>> shift_array_left<4>({0, 1, 2, 3, 4}, 2);
//     {0, 0, 0, 1}
void summary_examples(){}

//| Test nested examples
//
// #### __Example__
//
//     >>> some_example()
//     output
class nested_example
{
public:
  //|
  // #### __Example__
  //
  //     >>> some_nested_example()
  //     output
  const auto variable;
}

//| Test multiple examples
//
// #### __Example__
//
//     >>> one_example()
//     1
//
// #### __Example__
//
//     >>> two_example()
//     2
//
// #### __Example__
//
//     >>> three_example()
//     3
//
// A helpful paragraph between examples.
//
// #### __Example__
//
//     >>> four_example()
//     4
void multiple_examples(){}
