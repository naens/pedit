PRAGMA encoding = "UTF-8";
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS Language
(
	LanguageID INTEGER NOT NULL PRIMARY KEY,
	Name text
);

CREATE TABLE IF NOT EXISTS Text
(	
	TextID INTEGER NOT NULL PRIMARY KEY,
	LanguageID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (LanguageID) REFERENCES Language (LanguageID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS TextVersion
(
	TextVersionID INTEGER NOT NULL PRIMARY KEY,
	TextID INTEGER NOT NULL,
	Name text,
	PreChrs text,
	PostChrs text,
	SepChrs text,
	FOREIGN KEY (TextID) REFERENCES Text (TextID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Label
(	
	LabelID INTEGER NOT NULL PRIMARY KEY,
	TextNodeID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (TextNodeID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS TextNode
(
	TextNodeID INTEGER NOT NULL PRIMARY KEY,
	TextID INTEGER NOT NULL,
	FOREIGN KEY (TextID) REFERENCES Text (TextID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS TextNodeConnection
(
	TextNodeFromID INTEGER NOT NULL,
	TextNodeToID INTEGER NOT NULL,
	PRIMARY KEY (TextNodeFromID, TextNodeToID),
	FOREIGN KEY (TextNodeFromID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE,
	FOREIGN KEY (TextNodeToID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS TextCell
(
	TextVersionID INTEGER NOT NULL,
	TextNodeID INTEGER NOT NULL,
	Pre text,
	Post text,
	PRIMARY KEY (TextNodeID, TextVersionID),
	FOREIGN KEY (TextVersionID) REFERENCES TextVersion (TextVersionID) ON DELETE CASCADE,
	FOREIGN KEY (TextNodeID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Permutation
(
	TextVersionID INTEGER NOT NULL,
	TextNodeFromID INTEGER NOT NULL,
	TextNodeToID INTEGER NOT NULL,
	FOREIGN KEY (TextVersionID) REFERENCES TextVersion (TextVersionID) ON DELETE CASCADE,
	FOREIGN KEY (TextNodeFromID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE,
	FOREIGN KEY (TextNodeToID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS WordPart
(
	WordPartID INTEGER NOT NULL PRIMARY KEY,
	TextVersionID INTEGER NOT NULL,
	TextNodeID INTEGER NOT NULL,
	WordID INTEGER NOT NULL,
	TI_O int,
	Text text,
	FOREIGN KEY (TextVersionID) REFERENCES TextVersion (TextVersionID) ON DELETE CASCADE,
	FOREIGN KEY (TextNodeID) REFERENCES TextNode (TextNodeID) ON DELETE CASCADE,
	FOREIGN KEY (WordID) REFERENCES Word (WordID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Word
(
	WordID INTEGER NOT NULL PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS WordClass
(
	WordClassID INTEGER NOT NULL PRIMARY KEY,
	LanguageID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (LanguageID) REFERENCES Language (LanguageID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Lemma
(
	LemmaID INTEGER NOT NULL PRIMARY KEY,
	WordClassID INTEGER NOT NULL,
	Text text,
	FOREIGN KEY (WordClassID) REFERENCES WordClass (WordClassID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS WordLemma
(
	WordID INTEGER NOT NULL,
	LemmaID INTEGER NOT NULL,
	PRIMARY KEY (WordID, LemmaID),
	FOREIGN KEY (WordID) REFERENCES Word (WordID) ON DELETE CASCADE,
	FOREIGN KEY (LemmaID) REFERENCES Lemma (LemmaID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Category
(
	CategoryID INTEGER NOT NULL PRIMARY KEY,
	LanguageID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (LanguageID) REFERENCES Language (LanguageID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS CategoryValue
(
	CategoryValueID INTEGER NOT NULL PRIMARY KEY,
	CategoryID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (CategoryID) REFERENCES Category(CategoryID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS WordMovingValue
(
	WordID INTEGER NOT NULL,
	CategoryValueID INTEGER NOT NULL,
	PRIMARY KEY (WordID, CategoryValueID),
	FOREIGN KEY (WordID) REFERENCES Word (WordID) ON DELETE CASCADE,
	FOREIGN KEY (CategoryValueID) REFERENCES CategoryValue (CategoryValueID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS LemmaFixedValue
(
	LemmaID INTEGER NOT NULL,
	CategoryValueID INTEGER NOT NULL,
	PRIMARY KEY (LemmaID, CategoryValueID),
	FOREIGN KEY (LemmaID) REFERENCES Lemma (LemmaID) ON DELETE CASCADE,
	FOREIGN KEY (CategoryValueID) REFERENCES CategoryValue (CategoryValueID) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS WordClassCategory
(
	WordClassID INTEGER NOT NULL,
	CategoryID INTEGER NOT NULL,
	Fixed BOOLEAN NOT NULL,
	PRIMARY KEY (WordClassID, CategoryID),
	FOREIGN KEY (WordClassID) REFERENCES WordClass (WordClassID) ON DELETE CASCADE,
	FOREIGN KEY (CategoryID) REFERENCES Category (CategoryID) ON DELETE CASCADE
);
