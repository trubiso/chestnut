#include "levenshtein.hpp"
#include "resolver.hpp"

Diagnostic::Sample Resolver::get_type_sample(TypeInfo::ID id, OutFmt::Color color) const {
	return Diagnostic::Sample(
		get_context(get_type_file_id(id)),
		{Diagnostic::Sample::Label(get_type_span(id), get_type_name(id), color)}
	);
}

Resolver::TypeInfo::ID Resolver::type_next() {
	return type_counter_++;
}

Resolver::TypeInfo::ID
Resolver::register_type(TypeInfo&& type, Span span, FileContext::ID file_id, std::optional<AST::SymbolID> symbol_id) {
	TypeInfo::ID id = type_next();
	assert(type_pool_.size() == id);
	assert(type_span_pool_.size() == id);
	assert(type_symbol_mapping_.size() == id);
	type_pool_.push_back(std::move(type));
	type_span_pool_.push_back({span, file_id});
	type_symbol_mapping_.push_back(symbol_id);
	return id;
}

std::vector<AST::SymbolID> Resolver::get_operator_candidates(Token::Symbol operator_, bool binary) const {
	std::vector<AST::SymbolID> symbols {};
	for (Symbol const& symbol : symbol_pool_) {
		// we store operators with the name of the symbol for now (this will most likely change)
		if (symbol.name != get_variant_name(operator_)) continue;
		// this should never fail
		if (!type_pool_.at(symbol.type).is_function()) continue;
		if (type_pool_.at(symbol.type).get_function().arguments.size() != (binary ? 2 : 1)) continue;
		symbols.push_back(symbol.id);
	}
	return symbols;
}

void Resolver::infer_types() {
	for (ParsedFile& file : parsed_files) { infer(file.module, file.file_id); }
	decide_supposedly_known_named_types();
	decide_remaining_types();
}
