module A
  class Base
    class << self; attr_accessor :REQS end

    def self.REQ(kvs)
      kvs.keys.each do |a|
        self.send(:attr_accessor, a)
      end

      self.REQS = kvs.dup

      self
    end
  end
end

module A
  class Person < Base
    REQ(:foo => 108)
  end
end

module A
  class Berson < Person
    REQ(:bar => 108)
  end
end
