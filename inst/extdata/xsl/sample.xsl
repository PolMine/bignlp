<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="yes"/>

	<xsl:template match="/">
		<xsl:apply-templates select="/root"/>
	</xsl:template>
	
	<xsl:template match="root">
	  <xsl:element name = "root">
      <xsl:element name = "document">
		    <xsl:apply-templates select="//sentences"/>
      </xsl:element>
    </xsl:element>
	</xsl:template>

	<xsl:template match="sentences">
    <xsl:element name = "p">
      <xsl:apply-templates select="sentence"/>
    </xsl:element>
	</xsl:template>
	
	<xsl:template match="sentence">
    <xsl:element name = "s">
      <xsl:apply-templates select="./tokens/token"/>
    </xsl:element>
	</xsl:template>
	
	<xsl:template match="token">
    
    <xsl:element name = "w">
      
      <xsl:attribute name="id">
        <xsl:value-of select="./@id"/>
      </xsl:attribute>
      
      <xsl:attribute name="word">
        <xsl:value-of select="./word/text()"/>
      </xsl:attribute>
      
      <xsl:value-of select="./word/text()"/>

    </xsl:element>
      
	</xsl:template>
	
</xsl:stylesheet>
