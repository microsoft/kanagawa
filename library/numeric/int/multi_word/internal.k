// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.

module numeric.int.multi_word.internal
    { comp_metadata
    }

//| For use with min_with_metadata and max_with_metadata while computing min/max of a number represented through multiple words.
//  The struct represents the result of the computation on the "previous" word while progressing from the most significant word to the least.
struct comp_metadata
{
    //| The incoming metadata for comparing kth word (from the most significant word) of inputs `a_words` and `b_words`, will have its value equal to true
    //  if for all i in range [0, k), a_words[i] == b_words[i]. Note `0` here represents the most significant words' position.
    //  Initialize this to `true` for first invocation to `min_with_metadata` or `max_with_metadata`
    bool all_previous_words_eq;         

    //| The incoming metadata will have a valid value for this iff incoming `all_previous_words_eq` == `false`. 
    //  Once all_previous_words_eq is false, this variable represents that for all following words, words of `a_words`
    //  should be selected for the result.
    //  Can be initialized to either true or false.
    bool select_a_words_on_prevnoteq;   
}