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

	isNil,
        
        IVar,
        getIVar,
        setIVar,
        getAndSetIVar,
        getInstanceMVar,

        ClassMember(..),
        exportClass,
        
        NewlyAllocated,
        
        -- debugging & statistics:
        
        objectMapStatistics,
        
        -- things that shouldn't really be exported
        InstanceVariables,
        Typeable(..)
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

import Data.Typeable

