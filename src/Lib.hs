module Lib 
    ( runFoilLanguageServer
    , runBasicLanguageServer 
    , runAgnosticLambdaPiLS
    , runLambdaPiLSAlt
    , runFlanLS
    ) where

import BasicLS.BasicLS ( runBasicLanguageServer )
import FoilLS.FoilLS ( runFoilLanguageServer )
import AgnosticFoilLS.AgnosticLSLambdaPi ( runAgnosticLambdaPiLS )
import AgnosticFoilLSAlt.LambdaPiLSAlt ( runLambdaPiLSAlt )
import AgnosticLanguageServer.AgnosticLanguageServerFlan ( runFlanLS )
