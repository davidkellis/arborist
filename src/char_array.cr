# This is used as an alternative to String in cases where we need constant time random access and ranges of characters in the string.
class CharArray
  @chars : Array(Char)

  def initialize(str : String)
    @chars = str.each_char.to_a
  end

  def [](index : Int) : Char
    @chars[index]
  end

  def [](start_index : Int, count : Int) : String
    @chars[start_index, count].join

    # String.build do |str|
    #   @chars[start_index, count].each do |char|
    #     str << char
    #   end
    # end
  end

  def [](range : Range(Int32, Int32)) : String
    @chars[range].join
  end

  def size
    @chars.size
  end

  def to_s
    @chars.join
  end
end