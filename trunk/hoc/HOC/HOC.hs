module HOC (
        SEL,
        ID,
        nil,
        Object(..),
        Class,
        ClassAndObject,
        ( # ), ( #* ),
        ObjCArgument(..),
        castObject,
        declareClass,
        declareSelector,
        declareRenamedSelector,
        Covariant,
        CovariantInstance,
        Allocated,
        Inited,

        getSelectorForName,
        
        withAutoreleasePool,
        
        IVar,
        getIVar,
        setIVar,
        getAndSetIVar,
        getInstanceMVar,

        ClassMember(..),
        exportClass,
        
        NewlyAllocated,
        
        -- things that shouldn't really be exported
        InstanceVariables(..)
    ) where

import HOC.Base
import HOC.Arguments
import HOC.Invocation
import HOC.ID
import HOC.Class
import HOC.DeclareClass
import HOC.SelectorMarshaller
import HOC.DeclareSelector
import HOC.StdArgumentTypes
import HOC.ExportClass
import HOC.Utilities
import HOC.NewlyAllocated
