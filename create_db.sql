PRAGMA encoding = "UTF-8"; 

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
	FOREIGN KEY (LanguageID) REFERENCES Language(LanguageID)
);

CREATE TABLE IF NOT EXISTS TextVersion
(
	TextVersionID INTEGER NOT NULL PRIMARY KEY,
	TextID INTEGER NOT NULL,
	Name text,
	FOREIGN KEY (TextID) REFERENCES Text(TextID)
);

CREATE TABLE IF NOT EXISTS TextNode
(
	TextNodeID INTEGER NOT NULL PRIMARY KEY,
	TextID INTEGER NOT NULL,
	FOREIGN KEY (TextID) REFERENCES Text(TextID)
);

CREATE TABLE IF NOT EXISTS TextNodeConnection
(
	TextNodeFromID INTEGER NOT NULL,
	TextNodeToID INTEGER NOT NULL,
	PRIMARY KEY (TextNodeFromID, TextNodeToID),
	FOREIGN KEY (TextNodeFromID) REFERENCES TextNode(TextNodeID),
	FOREIGN KEY (TextNodeToID) REFERENCES TextNode(TextNodeID)
);

CREATE TABLE IF NOT EXISTS TextItem
(
	TextItemID INTEGER NOT NULL PRIMARY KEY,
	TextNodeID INTEGER NOT NULL,
	FOREIGN KEY (TextNodeID) REFERENCES TextNode(TextNodeID)
);

CREATE TABLE IF NOT EXISTS TextCell
(
	TextNodeID INTEGER NOT NULL,
	TextVersionID INTEGER NOT NULL,
	Pre text,
	Post text,
	PRIMARY KEY (TextNodeID, TextVersionID),
	FOREIGN KEY (TextNodeID) REFERENCES TextNode(TextNodeID),
	FOREIGN KEY (TextVersionID) REFERENCES TextVersion(TextVersionID)
);

	