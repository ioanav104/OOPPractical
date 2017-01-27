// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}
import Undoable.Change

/** The state of an editing session */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** What encryption is each element a part of. */
    private val crypt = new PlaneText()

    /** Number of ecrypted chunks of text. */
    private var cryptcount: Char = 0

    /** The display. */
    private var display: Display = null
    
    // State components that are preserver by undo and redo

    /** Current editing position. */
    private var _point = 0

    /** Current position of the mark. */
    private var _mark = 0
    
    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    // State components not preserved by undo and redo
  
    /** Number of changes */
    private var changes = 0


    /** Register a display */
    def register(display: Display) { this.display = display }

    /** Mark a new change */
    private def setModified() { changes = changes+1 }

    /** Test whether the text is modified */
    def isModified = (changes > 0)
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at point */
    def update() { update(point) }

    /** Update display with cursor at arbitrary position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }


    // Accessors

    def point = _point

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
    }

    def mark = _mark
   
    def mark_=(mark: Int) {
        _mark = mark
    }

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }


    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def isInWord(pos:Int) = text.isInWord(pos)
    
    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }

    def getEncryption(pos: Int) = { crypt.charAt(pos) }
    
    // Mutator methods

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.deleteChar(pos)
        crypt.deleteChar(pos)
        if(pos < mark) {
          mark = mark-1;
        }
        setModified()
        
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        text.deleteRange(pos, len)
        crypt.deleteRange(pos, len)
        if(pos < mark) {
            if(pos+len <= mark) {
              mark = mark-len
            } else {
              mark = pos
            }
        }
        setModified()
    }

    /** Insert a character */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.insert(pos, ch)
        crypt.insert(pos, 0.toChar)
        if(pos <= mark) mark = mark+1
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        val zero:Char = 0
        noteDamage(true)
        text.insert(pos, s)
        crypt.insert(pos,zero.toString * s.length) 
        if(pos <= mark) mark = mark+s.length
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        val zero: Char = 0
        noteDamage(true)
        text.insert(pos, s)
        crypt.insert(pos, zero.toString * s.length)
        if(pos <= mark) mark = mark+s.length
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        val zero: Char = 0
        noteDamage(true)
        text.insert(pos, t)
        crypt.insert(pos, zero.toString * t.length)
        if(pos <= mark) mark = mark+t.length
        setModified()
    }

    /** Traspose 2 characters. */
    def transpose(pos: Int) {
        noteDamage(true)
        var c = text.charAt(pos)
        text.deleteChar(pos)
        text.insert(pos+1, c)
        setModified()
    }

    /** Change a word that starts at pos to uppercase. */
    def toUpper(pos:Int) {
        noteDamage(true)
        var p = pos
        while(text.isInWord(p)) {
            if(text.charAt(p) >='a' && text.charAt(p) <='z') {
               val c = text.charAt(p)
               deleteChar(p)
               insert(p, (c+'A'-'a').toChar)
            }    
            p = p+1
        }
        setModified()
    }

    /** Encrypt a chunck of text. */
    def encrypt(left: Int, right: Int) {
      noteDamage(true)
      cryptcount = (cryptcount+1).toChar
        for(i <- left to right) {
            text.rot13(i)
            crypt.deleteChar(i)
            crypt.insert(i, cryptcount)
        }
    }

    /** Decrypt a part of a text, returning what it decryted. */
    def decrypt(pos: Int): (Int, Int) = {
        noteDamage(true)
        val c = getEncryption(pos)
        var right = pos
        while(getEncryption(right) == c) {
            text.rot13(right)
            crypt.deleteChar(right)
            crypt.insert(right, 0.toChar)
            right = right+1
        }
        var left = pos-1
        while(left >= 0 && getEncryption(left) == c) {
            text.rot13(left)
            crypt.deleteChar(left)
            crypt.insert(left, 0.toChar)
            left = left-1
        }
        return (left+1, right-1)
    }

    /** Load a file into the buffer. */
    def loadFile(name: String) {
        filename = name
        text.clear()
        
        try {
            val in = new FileReader(name)
            text.insertFile(0, in)
            in.close()
            crypt.insert(0, 0.toChar.toString * text.length)
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
        }
        
        changes = 0
        noteDamage(true)
    }
    
    /** Save contents on a file */
    def saveFile(name: String) {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            changes = 0
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write '%s'", name)
        }
    }


    /** Make a Memento that records the current editing state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point, the mark and the number 
     * of changes. */
    class Memento {
        private val pt = point
        
        private val mk = mark
       
        private val cs = changes

        /** Restore the state when the memento was created */
        def restore() { 
            point = pt 
            mark = mk
            changes = cs
        }
    }

    /** Change that records an insertion */
    class Insertion(pos: Int, text: Text.Immutable) extends Change {
        def undo() { deleteRange(pos, text.length) }
        def redo() { insert(pos, text) }
    }

    /** Insertion that can be amalgamated with adjacent, similar changes */
    class AmalgInsertion(val pos: Int, ch: Char) extends Change {
        /** The text inserted by all commands that have merged with this one */
        private val text = new Text(ch)

        def undo() { deleteRange(pos, text.length) }

        def redo() { insert(pos, text) }

        override def amalgamate(change: Change) = {
            change match {
                case other: AmalgInsertion =>
                    if (text.charAt(text.length-1) == '\n'
                            || other.pos != this.pos + this.text.length) 
                        false
                    else {
                        text.insert(text.length, other.text)
                        true
                    }

                case _ => false
            }
        }
    }

    class Encryption(enc: Char, left: Int, right: Int) extends Change {
        def undo() { assert( (left, right) == decrypt(left) ) }
        def redo() { 
            for(i <- left to right) {
                text.rot13(i)
                crypt.deleteChar(i)
                crypt.insert(i, enc)
            }
        }
    }

    class Decryption(enc: Char, left:Int, right: Int) extends Change {
        var encryption = enc
        def undo() {encrypt(left, right); encryption = getEncryption(left)}
        def redo() {decrypt(left);}
    
    }

    /** Change that records a deletion */
    class Deletion(pos: Int, deleted: Text) extends Change {
        def undo() { insert(pos, deleted) }
        def redo() { deleteRange(pos, deleted.length) }
    }

    /** Change that recordsa transposition */
    class Transposition(pos: Int) extends Change {
        def undo() { transpose(pos) }
        def redo() { transpose(pos) }
    }

    /** Change that records a word turned into uppercase */
    class Uppercase(pos: Int, word: String) extends Change {
        def undo() { deleteRange(pos, word.length); insert(pos, word) }
        def redo() { toUpper(pos) }
    } 

    /** The same word changed to uppercase, potentially from different positions
     *  can be amalgamated */
    class AmalgUppercase(pos: Int, word: String) extends Change {
        var original = word
        val position = pos
        def undo() { deleteRange(pos, original.length); insert(pos, original) }
        def redo() { toUpper(pos) }

        override def amalgamate(change: Change) = {
            change match {
                case other: AmalgUppercase => 
                  if(other.position == this.position) {
                      original = other.original
                      true
                  } else {
                      false
                  }
                case _ => false
            }
        }
    }

    def wrapChange(before: Memento, change: Change, after: Memento) = {
        if (change == null)
            null
        else
            new EditorChange(before, change, after)
    }

    /** Wrapper for text changes that preserves other state */
    class EditorChange(before: Memento, 
            private val change: Change,
            private var after: Memento) extends Change {

        def undo() {
            change.undo(); before.restore()
        }
            
        def redo() {
            change.redo(); after.restore()
        }
        
        def amalgamate(other: EditorChange) = {
            if (! change.amalgamate(other.change))
                false
            else {
                after = other.after
                true
            }
        }

        override def amalgamate(other: Change) =
            amalgamate(other.asInstanceOf[EditorChange])
    }
}

object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}
