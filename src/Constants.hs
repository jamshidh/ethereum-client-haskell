{-# OPTIONS_GHC  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Constants where


_Uether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Vether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000;
_Dether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000;
_Nether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Yether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000;
_Zether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000;
_Eether = 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Pether = 1000000000 * 1000000000 * 1000000000 * 1000000;
_Tether = 1000000000 * 1000000000 * 1000000000 * 1000
_Gether = 1000000000 * 1000000000 * 1000000000
_Mether = 1000000000 * 1000000000 * 1000000
_Kether = 1000000000 * 1000000000 * 1000
ether = 1000000000000000000
finney = 1000000000000000
szabo = 1000000000000
_Gwei = 1000000000
_Mwei = 1000000
_Kwei = 1000
wei = 1


blockDBPath::String
blockDBPath="/blocks/"

detailsDBPath::String
detailsDBPath="/details/"

stateDBPath::String
stateDBPath="/state/"

dbDir::Bool->String
--dbDir True = "/.ethereum"
dbDir True = "/Library/Application Support/Ethereum"
dbDir False = "/.ethereumH"


--"/Users/hutong/Library/Application Support/Ethereum/state/"
