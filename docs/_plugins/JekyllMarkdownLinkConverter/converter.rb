# We need to be placed in Kramdown::Converter.
# When calling a method on a Kramdown document of the form "to_****",
# Kramdown will shave off the "****" portion, Pascal-case it, and
# look for a class of that name in Kramdown::Converter.
# See https://github.com/gettalong/kramdown/blob/05d467bfb9abb732046e441ef1958471195d665d/lib/kramdown/document.rb#L113-L116
module Kramdown
    module Converter
        # Fixes relative markdown links to point to their dash-separated, lowercased html outputs
        class MarkdownLinkAmendedHtml < Html
            def convert_a(el, indent)
                href = el.attr['href']
                # Ensure that the link is relative to the site (doesn't start with "protocol://")
                # and that it links to a markdown file.
                if not /^\w+?:\/\// =~ href and href.end_with?('.md')
                    # Duplicate the attributes to avoid modifying the tree.
                    attr = el.attr.dup

                    # Remove the 'md', replace whitespace with dashes, switch the extension to html
                    dir, md_base = File.split(href)
                    html_base = md_base.chomp('.md').gsub(/\s+|\.|'/, '-').downcase + '.html'
                    attr['href'] = File.join(dir, html_base)

                    self.format_as_span_html(el.type, attr, self.inner(el, indent))
                else
                    super(el, indent)
                end
            end
        end
    end
end

# One requirement for Jekyll not to freak out
# is that we need to be in the Jekyll::Converters::Markdown
# module to be a valid markdown converter.
module Jekyll
    class Converters::Markdown::JekyllMarkdownLinkConverter < Converter
        safe true

        # Match markdown files.
        def matches(ext)
            ext =~ /^\.md$/i
        end

        # Output html files.
        def output_ext(ext)
            '.html'
        end

        def convert(content)
            kramdown_config = symbolize_keys(@config['kramdown'])
            doc = Kramdown::Document.new(content, kramdown_config)
            html = doc.to_markdown_link_amended_html
            return html;
        end
    end
end

def symbolize_keys(input)
    result = {}

    input.each do |k,v|
        result[k.intern] = v
    end

    result
end