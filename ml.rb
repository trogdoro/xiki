class Ml
  def self.<< txt
    File.open("/tmp/log.notes", "a") { |f| f << "#{txt}\n" }
    txt
  end
end
