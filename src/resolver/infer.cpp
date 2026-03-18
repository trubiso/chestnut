#include "levenshtein.hpp"
#include "resolver.hpp"

#include <sstream>

bool Resolver::Symbol::is_visible(FileContext::ID other_id) const {
	// if we're in the same file, it is trivially visible
	if (file_id == other_id) return true;
	// if it has been imported from the other file, it is visible
	if (std::find(imported_from.cbegin(), imported_from.cend(), other_id) != imported_from.cend()) return true;
	// otherwise, it is never visible
	return false;
}

AST::SymbolID Resolver::register_symbol(Symbol symbol, Span span, FileContext::ID file_id) {
	symbol.id      = symbol_next();
	symbol.span    = span;
	symbol.file_id = file_id;
	symbol_pool_.push_back(std::move(symbol));
}

Resolver::NameVar::ID Resolver::register_name(NameVar name, Span span, FileContext::ID file_id) {
	// span and file_id are unused, but they could prove useful later on, so we ask for them
	NameVar::ID id = name_next();
	name_pool_.push_back(std::move(name));
	return id;
}

Resolver::TypeVar::ID Resolver::register_type(TypeVar type, Span span, FileContext::ID file_id) {
	TypeVar::ID id = type_next();
	type_pool_.push_back(std::move(type));
	type_span_pool_.push_back({span, file_id});
	return id;
}

#define CLOSEST_THRESHOLD 3
#define CLOSEST_MAX       5

void Resolver::add_unknown_symbol_diagnostic(
	std::string_view                symbol,
	Span                            span,
	std::vector<std::string> const& possible_symbols,
	std::string_view                scope_type,
	FileContext::ID                 file_id,
	bool                            add_import_suggestion
) {
	std::string       title = std::format("unknown symbol `{}`", symbol);
	std::stringstream subtitle_stream {};
	subtitle_stream << "could not find any symbol with that name in the " << scope_type << " scope";

	std::vector<std::string_view> closest_symbols
		= closest(symbol, possible_symbols, CLOSEST_THRESHOLD, CLOSEST_MAX);

	if (!closest_symbols.empty()) {
		subtitle_stream << " (did you perhaps mean ";
		for (size_t i = 0; i < closest_symbols.size(); ++i) {
			subtitle_stream << '`' << closest_symbols[i] << '`';
			if (i + 1 < closest_symbols.size() - 1) subtitle_stream << ", ";
			else if (i + 1 < closest_symbols.size()) subtitle_stream << " or ";
		}
		subtitle_stream << "?)";
	}

	if (add_import_suggestion)
		subtitle_stream
			<< ". an unimported symbol with that name does exist, you might have forgotten to import it!";

	std::string subtitle = subtitle_stream.str();
	parsed_files.at(file_id).diagnostics.push_back(
		Diagnostic::error(
			std::move(title),
			std::move(subtitle),
			{Diagnostic::Sample(get_context(file_id), span)}
		)
	);
}

AST::Name
Resolver::create_name(std::string_view name, TypeVar type, bool mutable_, Span span, FileContext::ID file_id) {
	AST::SymbolID id = register_symbol(
		Symbol {0,
	                0,
	                Span(0),
	                std::string(name),
	                std::monostate {},
	                register_type(type, span, file_id),
	                mutable_,
	                false,
	                {}},
		span,
		file_id
	);
	AST::Name var {std::string(name)};
	var.id = {id};
	return var;
}

Resolver::TypeVar Resolver::from_type(AST::Type::Atom const& atom, FileContext::ID file_id) {
	switch (atom.kind()) {
	case AST::Type::Atom::Kind::Float: return TypeVar::make_float(TypeVar::KnownFloat {atom.get_float().width});
	case AST::Type::Atom::Kind::Void:  return TypeVar::make_void();
	case AST::Type::Atom::Kind::Char:  return TypeVar::make_char();
	case AST::Type::Atom::Kind::Bool:  return TypeVar::make_bool();
	case AST::Type::Atom::Kind::Named: {
		// FIXME: this does not resolve the identifier!!! we need to create all appropriate branches
		NameVar::ID name = register_name(NameVar(), atom.get_named().name.span, file_id);
		return TypeVar::make_named(name);
	}
	case AST::Type::Atom::Kind::Inferred: return TypeVar::make_unknown();
	case AST::Type::Atom::Kind::Integer:  {
		// for integers, we need to determine how much information we know
		AST::Type::Atom::Integer const& integer = atom.get_integer();
		if (integer.width_type() != AST::Type::Atom::Integer::WidthType::Any)
			return TypeVar::make_integer(TypeVar::KnownInteger {integer});
		else return TypeVar::make_partial_integer(TypeVar::PartialInteger {integer, true});
	}
	}
}

Resolver::TypeVar Resolver::from_type(AST::Type const& type, FileContext::ID file_id) {
	switch (type.kind()) {
	case AST::Type::Kind::Atom: return from_type(type.get_atom(), file_id);
	case AST::Type::Kind::Pointer:
		return TypeVar::make_pointer(
			register_type(
				from_type(type.get_pointer().type->value, file_id),
				type.get_pointer().type->span,
				file_id
			),
			type.get_pointer().mutable_
		);
	}
}

void Resolver::infer() {}
