module HOC.Utilities where

import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.TH
import HOC.ExportClass
import Foreign.Ptr

x # f = f x

x #. v = x # getIVar v

declareMarshalledObjectType ty
    = do
        (context, ty') <- splitTy ty
        argInst <- instanceD context (conT ''ObjCArgument
            `appT` ty' `appT` [t| Ptr ObjCObject |])
                `whereQ` valDs [
                    ('withExportedArgument,   [| withExportedArgument   . toID |]),
                    ('exportArgument,         [| exportArgument         . toID |]),
                    ('exportArgumentRetained, [| exportArgumentRetained . toID |]),
                    ('importArgument,         [| fmap fromID . importArgument    |]),
                    ('objCTypeString,         [| objCTypeString         . toID |])
                ]
        msgTarget <- instanceD context (conT ''MessageTarget
            `appT` ty')
                `whereQ` valDs [
                    ('isNil, [| \_ -> False |]),
                    ('sendMessageWithRetval,    [| sendMessageWithRetval    . toID |]),
                    ('sendMessageWithoutRetval, [| sendMessageWithoutRetval . toID |])
                ]
        return [argInst, msgTarget]
    where
        valDs decls
            = sequence [
                do e <- b ; return (ValD (VarP n) (NormalB e) [])
                | (n, b) <- decls
            ]
        
        splitTy ty = do t <- ty
                        return $ case t of
                            (ForallT ns context t') -> (return context, return t')
                            other                   -> (cxt [], ty)