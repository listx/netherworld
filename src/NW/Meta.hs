module NW.Meta where

_prog_name
	, _prog_version
	, _prog_info
	, _prog_info'
	, _prog_summary
	, _copyright
	:: String

_prog_name = "NetherWorld"
_prog_version = "0.0.1"
_prog_info = _prog_name ++ " version " ++ _prog_version
_prog_info' = _prog_name ++ " " ++ _prog_version
_prog_summary = "a simple RPG game"
_copyright = "(C) Linus Arver 2014"

_about_msg :: String
_about_msg = init . unlines $
	[ _prog_info'
	, _copyright
	, "<" ++ x ++ "@" ++ y ++ "." ++ z ++ ">"
	]
	where
	x = "linus"
	y = "ucla"
	z = "edu"
