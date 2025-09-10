//| [To custom-anchor](#custom-anchor)
module sample.links
    { module sample.decls.initializer
    , linked_bool_alias
    , array_alias
    , module sample.decls
    , CrazyNumbers
    , bool_alias
    , exposed_alias
    }

import sample.decls
import sample.decls.initializer

//| []{#custom-anchor .anchor}
using linked_bool_alias = bool_alias;

using unexposed_alias = uint32;
using exposed_alias = unexposed_alias;
