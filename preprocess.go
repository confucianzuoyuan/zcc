package main

// Entry point function of the preprocessor.
func preprocess(tok *Token) *Token {
	convertKeywords(tok)
	return tok
}
