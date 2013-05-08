class Dimensions
  def self.menu_after output, *args

    # /, so don't interfere...

    return if args.blank?

    View.kill if View.name == "@dimensions/"
    nil
  end
end
