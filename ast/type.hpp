#pragma once
#include <cstdint>
#include <iostream>
#include <optional>
#include <variant>

namespace AST {

struct Type {
	struct Atom {
		// TODO: eventually support types which are identifiers. for now, we won't, as there is no way to create
		// them.

		class Integer {
			uint32_t width;
			bool signed_;  // TODO: maybe express this as a bit in width, although that's kinda ridiculous

			static uint32_t const ANY  = (1 << 23) + 1;
			static uint32_t const PTR  = (1 << 23) + 2;
			static uint32_t const SIZE = (1 << 23) + 3;

			explicit Integer(uint32_t width, bool signed_) : width {width}, signed_ {signed_} {}

			static inline constexpr bool is_valid_user_width(uint32_t w) { return w < (1 << 23); }

			static inline constexpr bool is_valid_width(uint32_t w) {
				return is_valid_user_width(w) || w == ANY || w == PTR || w == SIZE;
			}

		public:
			enum class WidthType {
				Fixed,
				Any,
				Ptr,
				Size,
			};

			static inline std::optional<Integer> with_width(uint32_t width, bool signed_) {
				if (!is_valid_width(width)) return {};
				else return Integer {width, signed_};
			}

			static inline Integer any(bool signed_) { return Integer {ANY, signed_}; }

			static inline Integer ptr(bool signed_) { return Integer {PTR, signed_}; }

			static inline Integer size(bool signed_) { return Integer {SIZE, signed_}; }

			inline WidthType width_type() const {
				switch (width) {
				case ANY:  return WidthType::Any;
				case PTR:  return WidthType::Ptr;
				case SIZE: return WidthType::Size;
				default:   return WidthType::Fixed;
				}
			}

			inline bool is_signed() const { return signed_; }

			inline std::optional<uint32_t> bit_width() const {
				return width_type() == WidthType::Fixed ? std::optional {width} : std::nullopt;
			}
		};

		struct Float {
			enum class Width { F16, F32, F64, F128 } width;

			inline constexpr uint8_t width_value() const {
				switch (width) {
				case Width::F16:  return 16;
				case Width::F32:  return 32;
				case Width::F64:  return 64;
				case Width::F128: return 128;
				}
			}
		};

		enum class Kind {
			Integer = 0,
			Float   = 1,
			Void    = 2,
			Char    = 3,
			Bool    = 4,
		};

		typedef std::variant<Integer, Float, std::monostate, std::monostate, std::monostate> value_t;

		value_t value;

		inline constexpr Kind kind() const { return (Kind) value.index(); }

		// TODO: would be beautiful to codegen all of this boilerplate, but alas,

		inline static Atom make_integer(Integer&& integer) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Integer>, integer});
		}

		inline static Atom make_float(Float::Width width) {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Float>, Float {width}});
		}

		inline static Atom make_void() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Void>, std::monostate {}});
		}

		inline static Atom make_char() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Char>, std::monostate {}});
		}

		inline static Atom make_bool() {
			return Atom(value_t {std::in_place_index<(size_t) Kind::Bool>, std::monostate {}});
		}

		inline Integer const& get_integer() const { return std::get<(size_t) Kind::Integer>(value); }

		inline Float get_float() const { return std::get<(size_t) Kind::Float>(value); }
	};

	enum class Kind {
		Atom = 0,
	};

	typedef std::variant<Atom> value_t;

	value_t value;

	inline constexpr Kind kind() const { return (Kind) value.index(); }

	inline static Type make_atom(Atom&& atom) {
		return Type(value_t {std::in_place_index<(size_t) Kind::Atom>, atom});
	}

	inline Atom const& get_atom() const { return std::get<(size_t) Kind::Atom>(value); }
};

std::ostream& operator<<(std::ostream&, Type::Atom const&);
std::ostream& operator<<(std::ostream&, Type const&);

}  // namespace AST
