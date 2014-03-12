<?xml version='1.0'?>
<!--
 * XSL converter script for db_erlang mocked databases
 *
 * Copyright (C) 2001-2007 FhG Fokus
 *
 * This file is part of Kamailio, a free SIP server.
 *
 * Kamailio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version
 *
 * Kamailio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'
                xmlns:xi="http://www.w3.org/2001/XInclude"
>
   <xsl:template match="/">
	<xsl:variable name="createfile" select="$dest"/>
	<xsl:document href="{$createfile}" method="text" indent="no" omit-xml-declaration="yes">
	    <xsl:text>-module(schema).&#x0A;</xsl:text>
	    <xsl:text>-export([schema/1,version/1]).&#x0A;</xsl:text>
	    <xsl:text>&#x0A;</xsl:text>
	    <xsl:apply-templates select="foo[1]"/>
<!--	    <xsl:apply-templates select="database"/>-->
	</xsl:document>
    </xsl:template>

    <xsl:template match="foo">
	<xsl:apply-templates select="database"/>
	<xsl:apply-templates select="user"/>
	<!-- Create row in version table -->
	<xsl:for-each select="database/table">
	    <xsl:if test="version">
		<xsl:apply-templates select="version"/>
		<xsl:choose>
		    <xsl:when test="not(position()=last())">
			<xsl:text>;&#x0A;</xsl:text>
		    </xsl:when>
		    <xsl:otherwise>
			<xsl:text>.&#x0A;</xsl:text>
		    </xsl:otherwise>
		</xsl:choose>
	    </xsl:if>
	</xsl:for-each>
    </xsl:template>
        

<!-- ************************************************************ -->
    <xsl:key name="column_id" match="column" use="@id|xml:id"/>

    <xsl:param name="prefix" select="_"/>
    <xsl:param name="dir" select="mm"/>
    <xsl:param name="db" select="_"/>

    <xsl:variable name="sign-prefix">unsigned </xsl:variable>

    <!-- Do not output text -->
    <xsl:template match="text()|@*"/>
    <xsl:template match="text()|@*" mode="drop"/>

    <xsl:template name="get-name">
	<xsl:param name="select" select="."/>
	<xsl:choose>
	    <!-- override test -->
	    <xsl:when test="count($select/name[@db=$db])='1'">
		<xsl:value-of select="normalize-space($select/name[@db=$db])"/>
	    </xsl:when>
	    <!-- No override, use the standard name -->
	    <xsl:otherwise>
		<xsl:value-of select="normalize-space($select/name)"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <xsl:template name="type-error">
	<xsl:message terminate="yes">
	    <xsl:text>ERROR: Table: </xsl:text>
	    <xsl:value-of select="normalize-space(parent::table/name)"/>
	    <xsl:text>, column: </xsl:text>
	    <xsl:value-of select="normalize-space(name)"/>
	    <xsl:text> - unsupported column type: </xsl:text>
	    <xsl:value-of select="normalize-space(type)"/>
	    <xsl:text>.</xsl:text>
	</xsl:message>
    </xsl:template>

    <!-- ################ DATABASE ################# -->

    <xsl:template match="database">
	<xsl:variable name="database.name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:text>%% </xsl:text>
	<xsl:value-of select="$database.name"/>
	<xsl:text>&#x0A;</xsl:text>

	<!-- Create all tables -->
	<xsl:if test="table">
	    <xsl:apply-templates select="table"/>
	    <xsl:choose>
		<xsl:when test="not(position()=last())">
		    <xsl:text>;&#x0A;</xsl:text>
		</xsl:when>
		<xsl:otherwise>
		    <xsl:text>.&#x0A;</xsl:text>
		</xsl:otherwise>
	    </xsl:choose>
	</xsl:if>
	<xsl:text>&#x0A;</xsl:text>
	
<!--	<xsl:apply-templates select="user"/> -->
    </xsl:template> 

<!--    <xsl:template match="database" mode="data">

	<!- - Insert initial data - ->
	<xsl:apply-templates select="table" mode="data"/>
    </xsl:template>
-->
    <!-- ################ /DATABASE ################# -->

    <!-- ################ TABLE ################# -->

    <xsl:template match="table">
	<xsl:variable name="table.name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>

	<xsl:text>schema(</xsl:text>
	<xsl:value-of select="$table.name"/>
	<xsl:text>) -> [</xsl:text>

	<!-- Process all columns -->
	<xsl:apply-templates select="column"/>

	<!-- Process all unique indexes - - >
	<xsl:apply-templates select="index[child::unique]"/>
-->
	<!-- Process all primary indexes - - >
	<xsl:apply-templates select="index[child::primary]"/>

	<xsl:text>&#x0A;</xsl:text>
-->
	<xsl:text>]</xsl:text>
<!--	<xsl:if test="type[@db=$db]">
	    <xsl:text> Type=</xsl:text>
	    <xsl:value-of select="normalize-space(type[@db=$db])"/>
	</xsl:if> -->
	<xsl:if test="not(position()=last())">
	    <xsl:text>;&#x0A;</xsl:text>
	</xsl:if>
	<!-- Process all indexes -->
<!--	<xsl:apply-templates select="index"/> -->
<!--	<xsl:for-each select="index[count(child::unique)=0]">
	    <xsl:if test="not(child::primary)">
	        <xsl:call-template name="create_index"/>
	    </xsl:if>
	</xsl:for-each> -->
    </xsl:template>

    <!-- ################ /TABLE ################# -->

    <!-- ################ COLUMN ################# -->

    <xsl:template match="column"/>

    <xsl:template name="get-type-string">
	<xsl:param name="select" select="."/>
	<xsl:choose>
	    <xsl:when test="count($select/type[@db=$db])='1'">
		<xsl:value-of select="translate(normalize-space($select/type[@db=$db]),
		  'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:value-of select="translate(normalize-space($select/type),
		  'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <xsl:template name="get-type">
	<xsl:param name="select" select="."/>
	<xsl:variable name="type">
	    <xsl:call-template name="get-type-string">
		<xsl:with-param name="select" select="$select"/>
	    </xsl:call-template>
	</xsl:variable>
	<xsl:choose>
	    <xsl:when test="starts-with($type, $sign-prefix)">
		<xsl:value-of select="substring-after($type, $sign-prefix)"/>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:value-of select="$type"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <xsl:template name="get-sign">
	<xsl:param name="select" select="."/>
	<xsl:variable name="type">
	    <xsl:call-template name="get-type-string">
		<xsl:with-param name="select" select="$select"/>
	    </xsl:call-template>
	</xsl:variable>
	<xsl:choose>
	    <xsl:when test="starts-with($type, $sign-prefix)">0</xsl:when>
	    <xsl:otherwise>1</xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <xsl:template name="get-null">
	<xsl:param name="select" select="."/>
	<xsl:choose>
	    <xsl:when test="count($select/null[@db=$db])='1'">1</xsl:when>
	    <xsl:when test="count($select/null)='1'">1</xsl:when>
	    <xsl:otherwise>0</xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <xsl:template name="get-size">
	<xsl:param name="select" select="."/>
	<xsl:choose>
	    <xsl:when test="count($select/size[@db=$db])='1'">
		<xsl:value-of select="normalize-space($select/size[@db=$db])"/>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:value-of select="normalize-space($select/size)"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>


    <!-- column ID to column name -->
    <xsl:template name="get-column-name">
	<xsl:param name="select" select="."/>

	<xsl:variable name="columns" select="key('column_id', $select)"/>
	<xsl:variable name="column" select="$columns[1]"/>
	<xsl:choose>
	    <xsl:when test="count($column) = 0">
		<xsl:message terminate="yes">
		    <xsl:text>ERROR: Column with id '</xsl:text>
		    <xsl:value-of select="$select"/>
		    <xsl:text>' does not exist.</xsl:text>
		</xsl:message>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="get-name">
		    <xsl:with-param name="select" select="$column"/>
		</xsl:call-template>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>


    <xsl:template name="get-column">
	<xsl:param name="id" select="/.."/>	
	<xsl:variable name="columns" select="key('column_id', $id)"/>
	<xsl:variable name="column" select="$columns[1]"/>

	<xsl:choose>
	    <xsl:when test="count($column) = 0">
		<xsl:message terminate="yes">
		    <xsl:text>ERROR: Column with id '</xsl:text>
		    <xsl:value-of select="$id"/>
		    <xsl:text>' does not exist.</xsl:text>
		</xsl:message>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:copy-of select="$column"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

    <!-- ################ /COLUMN ################# -->
<!-- ************************************************************-->





<!-- ################ VERSION ################  -->

    <xsl:template match="version">
	<xsl:text>version("</xsl:text>
	<xsl:call-template name="get-name">
		<xsl:with-param name="select" select="parent::table"/>
	</xsl:call-template>
	<xsl:text>") -> </xsl:text>
	<xsl:value-of select="text()"/>
    </xsl:template>

<!-- ################ /VERSION ################  -->


<!-- ################ INDEX (constraint) ################  -->

    <xsl:template match="index">
	<xsl:variable name="index.name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>

	<xsl:if test="position()=1">
	    <xsl:text>,&#x0A;</xsl:text>
	</xsl:if>
	<xsl:text>    </xsl:text>
	<xsl:if test="not($index.name='')">
		<xsl:text>CONSTRAINT </xsl:text>
		<xsl:call-template name="get-index-name"/>
	</xsl:if>
	<xsl:if test="unique">
		<xsl:text> UNIQUE (</xsl:text>
		<xsl:apply-templates select="colref"/>
		<xsl:text>)</xsl:text>
	    <xsl:if test="not(position()=last())">
		<xsl:text>,</xsl:text>
		<xsl:text>&#x0A;</xsl:text>
	    </xsl:if>
	</xsl:if>
	<!-- PRIMARY KEY standalone definition -->
	<xsl:if test="primary">
	    <xsl:text> PRIMARY KEY </xsl:text>
	    <xsl:text> (</xsl:text>
	    <xsl:apply-templates select="colref"/>
	    <xsl:text>)</xsl:text>
	    <xsl:if test="not(position()=last())">
		<xsl:text>,</xsl:text>
		<xsl:text>&#x0A;</xsl:text>
	    </xsl:if>
	</xsl:if>
    </xsl:template>

<!-- ################ /INDEX (constraint) ################  -->

<!-- ################ INDEX (create) ################  -->

    <xsl:template name="create_index">
	<xsl:variable name="index.name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:variable name="table.name">
	    <xsl:call-template name="get-name">
		<xsl:with-param name="select" select="parent::table"/>
	    </xsl:call-template>
	</xsl:variable>

	<xsl:text>CREATE </xsl:text>
	<xsl:if test="unique">
	    <xsl:text>UNIQUE </xsl:text>
	</xsl:if>
	<xsl:text>INDEX </xsl:text>
	<xsl:if test="not($index.name='')">
		<xsl:call-template name="get-index-name"/>
	</xsl:if>
	<xsl:text> ON </xsl:text>
	<xsl:value-of select="$table.name"/>
	<xsl:text> (</xsl:text>
	<xsl:apply-templates select="colref"/>
	<xsl:text>);&#x0A;</xsl:text>

	<xsl:if test="position()=last()">
	    <xsl:text>&#x0A;</xsl:text>
	</xsl:if>
    </xsl:template>

<!-- ################ /INDEX (create) ################  -->


<!-- ################ COLUMN ################  -->

    <xsl:template match="column">
	<xsl:text>{</xsl:text>
	<xsl:call-template name="get-name"/>
	<xsl:text>, </xsl:text>

	<xsl:call-template name="column.type"/>
	<xsl:text>, </xsl:text>
	<xsl:call-template name="column.size"/>
	<xsl:text>, </xsl:text>
	
	<xsl:choose>
	    <xsl:when test="default[@db=$db]">
		<xsl:choose>
		    <xsl:when test="default[@db=$db]/null">
			<xsl:text>null</xsl:text>
		    </xsl:when>
		    <xsl:otherwise>
			<xsl:value-of select="default[@db=$db]"/>
		    </xsl:otherwise>
		</xsl:choose>
	    </xsl:when>
	    <xsl:when test="default">
		<xsl:choose>
		    <xsl:when test="default/null">
			<xsl:text>null</xsl:text>
		    </xsl:when>
		    <xsl:when test="string(number(default))='NaN'"><!-- test for string value -->
			<xsl:text>"</xsl:text>
			<xsl:value-of select="default"/>
			<xsl:text>"</xsl:text>
		    </xsl:when>
		    <xsl:otherwise>
			<xsl:value-of select="default"/><!-- ommit the quotes for numbers -->
		    </xsl:otherwise>
		</xsl:choose>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:text>nodefault</xsl:text>
	    </xsl:otherwise>
	</xsl:choose>
	<xsl:text>, </xsl:text>
	
	<xsl:variable name="null">
	    <xsl:call-template name="get-null"/>
	</xsl:variable>
	<xsl:choose>
	    <xsl:when test="$null=0">
		<xsl:text>notnull</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:text>nullok</xsl:text>
	    </xsl:otherwise>
	</xsl:choose>
	<xsl:text>}</xsl:text>

	<xsl:if test="not(position()=last())">
	    <xsl:text>, </xsl:text>
	</xsl:if>
    </xsl:template>

    <xsl:template name="column.type">
	<xsl:variable name="type">
	    <xsl:call-template name="get-type"/>
	</xsl:variable>

	<xsl:choose>
	    <xsl:when test="type[@db=$db]">
		<xsl:value-of select="normalize-space(type[@db=$db])"/>
	    </xsl:when>
	    <xsl:when test="$type='char'">
		<xsl:text>char</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='short'">
		<xsl:text>short</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='int'">
			<xsl:if test="not(autoincrement)">
				<xsl:text>int</xsl:text>
			</xsl:if>
			<xsl:if test="autoincrement">
				<xsl:text>autoint</xsl:text>
			</xsl:if>
	    </xsl:when>
	    <xsl:when test="$type='long'">
		<xsl:text>long</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='datetime'">
		<xsl:text>datetime</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='double'">
		<xsl:text>double</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='float'">
		<xsl:text>float</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='string'">
		<xsl:text>string</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='binary' or
						$type='largebinary'">
		<xsl:text>binary</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='text'or
						$type='largetext'">
		<xsl:text>text</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="type-error"/>
	    </xsl:otherwise>
	</xsl:choose>
	</xsl:template>


    <xsl:template name="column.size">
	<xsl:variable name="size">
	    <xsl:call-template name="get-size"/>
	</xsl:variable>
	<xsl:choose>
	    <xsl:when test="not($size='')">
		<xsl:value-of select="$size"/>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:text>any</xsl:text>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

<!-- ################ /COLUMN ################  -->


<!-- ################ COLREF ################  -->

    <xsl:template match="colref">
	<xsl:call-template name="get-column-name">
	    <xsl:with-param name="select" select="@linkend"/>
	</xsl:call-template>
	<xsl:if test="not(position()=last())">
	    <xsl:text>, </xsl:text>
	</xsl:if>
    </xsl:template>

<!-- ################ /COLREF ################  -->


    <xsl:template name="get-index-name">
	<xsl:variable name="index.name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:variable name="table.name">
	    <xsl:call-template name="get-name">
		<xsl:with-param name="select" select="parent::table"/>
	    </xsl:call-template>
	</xsl:variable>
	<!-- because postgres don't like identical index names, even on table level -->
	<xsl:value-of select="concat($table.name, '_', $index.name)"/>
    </xsl:template>

</xsl:stylesheet>
